-module(dummy_worker).

-export([
    start_worker_monitor/1,
    stop_worker/1,
    spindown/1,
    spindown_with/2,
    init/1,
    handle_cast/2,
    handle_info/2
]).

start_worker_monitor(Key) ->
    ServerName =
        case Key of
            1 -> worker1;
            2 -> worker2
        end,
    gen_server:start_monitor({local, ServerName}, ?MODULE, Key, []).

stop_worker(Pid) ->
    gen_server:stop(Pid).

spindown(ServerName) ->
    spindown_with(ServerName, spindown).

spindown_with(ServerName, Reason) ->
    gen_server:cast(ServerName, {spindown, Reason}).

init(Key) ->
    {ok, #{key => Key}}.

handle_cast(spindown, State) ->
    {stop, spindown, State};
handle_cast({spindown, Reason}, State) ->
    {stop, Reason, State}.

handle_info({Ref, From, req}, State) ->
    From ! {From, Ref, ok},
    {noreply, State}.
