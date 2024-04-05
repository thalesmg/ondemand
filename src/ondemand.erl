-module(ondemand).

%% API
-export([request/4]).

request(Manager, ResolveFn, Key, Request) ->
    case ResolveFn(Key) of
        undefined ->
            ondemand_manager:ensure_started(Manager, Key),
            request(Manager, ResolveFn, Key, Request);
        Pid when is_pid(Pid) ->
            {Ref, Alias} = monitor_alias(Pid),
            Pid ! {Ref, Alias, Request},
            receive
                {Alias, Ref, Resp} ->
                    demonitor(Ref, [flush]),
                    Resp;
                {'DOWN', Ref, process, Pid, noproc} ->
                    un_alias(Alias),
                    ondemand_manager:ensure_started(Manager, Key),
                    request(Manager, ResolveFn, Key, Request);
                {'DOWN', Ref, process, Pid, spindown} ->
                    un_alias(Alias),
                    ondemand_manager:ensure_started(Manager, Key),
                    request(Manager, ResolveFn, Key, Request);
                {'DOWN', Ref, process, Pid, {shutdown, spindown}} ->
                    un_alias(Alias),
                    ondemand_manager:ensure_started(Manager, Key),
                    request(Manager, ResolveFn, Key, Request);
                {'DOWN', Ref, process, Pid, Reason} ->
                    un_alias(Alias),
                    error({worker_died, Reason})
            end
    end.

-ifdef(CONCUERROR).
monitor_alias(Pid) ->
    Ref = monitor(process, Pid),
    {Ref, self()}.

un_alias(_Alias) ->
    ok.
-else.
monitor_alias(Pid) ->
    Ref = monitor(process, Pid, [{alias, reply_demonitor}]),
    {Ref, Ref}.

un_alias(Alias) ->
    unalias(Alias).
-endif.
