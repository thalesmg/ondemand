-module(ondemand).

%% API
-export([request/4]).

request(Manager, ResolveFn, Key, Request) ->
    case ResolveFn(Key) of
        undefined ->
            ondemand_manager:ensure_started(Manager, Key),
            request(Manager, ResolveFn, Key, Request);
        Pid when is_pid(Pid) ->
            Ref = monitor(process, Pid),
            Pid ! {Ref, self(), Request},
            receive
                {Ref, Resp} ->
                    demonitor(Ref, [flush]),
                    Resp;
                {'DOWN', Ref, process, Pid, noproc} ->
                    ondemand_manager:ensure_started(Manager, Key),
                    request(Manager, ResolveFn, Key, Request);
                {'DOWN', Ref, process, Pid, spindown} ->
                    ondemand_manager:ensure_started(Manager, Key),
                    request(Manager, ResolveFn, Key, Request);
                {'DOWN', Ref, process, Pid, Reason} ->
                    error({worker_died, Reason})
            end
    end.
