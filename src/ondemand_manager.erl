-module(ondemand_manager).

%% API
-export([
    start_link/1,
    start_link/2,
    ensure_started/2
]).

%% `gen_server' API
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(spinup, {key}).

ensure_started(Manager, Key) ->
    gen_server:call(Manager, #spinup{key = Key}, infinity).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

start_link(ServerName, Opts) ->
    gen_server:start_link(ServerName, ?MODULE, Opts, []).

init(Opts) ->
    process_flag(trap_exit, true),
    #{
        mfa := {Mod, Fn, Args},
        terminate_mfa := {ModT, FnT, ArgsT}
    } = Opts,
    State = #{
        flagged => #{},
        mfa => {Mod, Fn, Args},
        refs => #{},
        terminate_mfa => {ModT, FnT, ArgsT},
        workers => #{}
    },
    {ok, State}.

terminate(Reason, State) ->
    #{
        terminate_mfa := {Mod, Fn, Args},
        flagged := Flagged,
        workers := Workers
    } = State,
    maps:foreach(
        fun(_Key, #{pending := Pending}) ->
            lists:foreach(
                fun(From) ->
                    gen_server:reply(From, {error, manager_shutting_down, Reason})
                end,
                Pending
            )
        end,
        Flagged
    ),
    maps:foreach(
        fun(_Key, {Pid, _Ref}) ->
            catch apply(Mod, Fn, [Pid | Args])
        end,
        Workers
    ),
    ok.

handle_call(#spinup{key = Key}, From, State0) ->
    #{flagged := Flagged0} = State0,
    case maps:get(Key, Flagged0, false) of
        false ->
            self() ! {ensure, Key},
            Flagged = Flagged0#{Key => #{pending => [From]}},
            State = State0#{flagged := Flagged};
        #{pending := Pending0} = KPending ->
            Flagged = Flagged0#{Key := KPending#{pending := [From | Pending0]}},
            State = State0#{flagged := Flagged}
    end,
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({ensure, Key}, State0) ->
    #{
        workers := Workers0,
        refs := Refs0,
        flagged := Flagged
    } = State0,
    #{pending := Pending} = maps:get(Key, Flagged),
    case maps:get(Key, Workers0, undefined) of
        undefined ->
            State = do_spinup(State0, Key);
        {Pid, Ref} ->
            case is_process_alive(Pid) of
                true ->
                    lists:foreach(
                        fun(From) ->
                            gen_server:reply(From, ok)
                        end,
                        Pending
                    ),
                    State = State0#{flagged := maps:remove(Key, Flagged)};
                false ->
                    demonitor(Ref, [flush]),
                    State1 = State0#{
                        refs := maps:remove({Pid, Ref}, Refs0),
                        workers := maps:remove(Key, Workers0)
                    },
                    State = do_spinup(State1, Key)
            end
    end,
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, _Reason}, State0 = #{workers := Workers0}) when
    is_map_key({Pid, Ref}, Workers0)
->
    State = State0#{workers := maps:remove({Pid, Ref}, Workers0)},
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

do_spinup(State0, Key) ->
    #{
        mfa := {Mod, Fn, Args},
        flagged := Flagged,
        workers := Workers0,
        refs := Refs0
    } = State0,
    #{pending := Pending} = maps:get(Key, Flagged),
    case apply(Mod, Fn, [Key | Args]) of
        {ok, {Pid, Ref}} when is_pid(Pid), is_reference(Ref) ->
            lists:foreach(
                fun(From) ->
                    gen_server:reply(From, ok)
                end,
                Pending
            ),
            State0#{
                flagged := maps:remove(Key, Flagged),
                workers := Workers0#{Key => {Pid, Ref}},
                refs := Refs0#{{Pid, Ref} => true}
            };
        Error ->
            lists:foreach(
                fun(From) ->
                    gen_server:reply(From, {error, Error})
                end,
                Pending
            ),
            State0#{flagged := maps:remove(Key, Flagged)}
    end.
