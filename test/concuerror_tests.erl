-module(concuerror_tests).

-compile([export_all, nowarn_export_all]).

start_manager() ->
    Opts = #{
        mfa => {dummy_worker, start_worker_monitor, []},
        terminate_mfa => {dummy_worker, stop_worker, []}
    },
    ondemand_manager:start_link({local, manager}, Opts).

stop_manager() ->
    gen_server:stop(manager).

resolve(1) -> whereis(worker1);
resolve(2) -> whereis(worker2).

request(Key) ->
    ondemand:request(manager, fun resolve/1, Key, req).

spinup_spindown_test() ->
    start_manager(),
    Key = 1,
    Me = self(),
    Req = fun() ->
        Resp = request(Key),
        Me ! {done, Resp}
    end,
    spawn(Req),
    spawn(Req),
    spawn(fun() ->
        dummy_worker:spindown(resolve(Key))
    end),
    receive
        {done, ok} -> ok
    end,
    receive
        {done, ok} -> ok
    end,
    stop_manager(),
    ok.

spindown_already_up_test() ->
    start_manager(),
    Key = 1,
    ondemand_manager:ensure_started(manager, Key),
    Ref = monitor(process, resolve(Key)),
    Me = self(),
    Req = fun() ->
        Resp = request(Key),
        Me ! {done, Resp}
    end,
    spawn(Req),
    spawn(Req),
    spawn(fun() ->
        dummy_worker:spindown(resolve(Key))
    end),
    receive
        {'DOWN', Ref, process, _, _} ->
            ok
    end,
    receive
        {done, ok} -> ok
    end,
    receive
        {done, ok} -> ok
    end,
    stop_manager(),
    ok.

unknown_exit_test() ->
    start_manager(),
    Key = 1,
    Me = self(),
    TryReq = fun() ->
        try
            Resp = request(Key),
            Me ! {done, Resp}
        catch
            error:{worker_died, boom} ->
                Me ! {done, died}
        end
    end,
    spawn(TryReq),
    spawn(TryReq),
    spawn(fun() ->
        dummy_worker:spindown_with(resolve(Key), boom)
    end),
    receive
        {done, ok} -> ok;
        {done, died} -> ok
    end,
    receive
        {done, ok} -> ok;
        {done, died} -> ok
    end,
    stop_manager(),
    ok.

two_workers_test() ->
    start_manager(),
    Me = self(),
    Req = fun(Key) ->
        Resp = request(Key),
        Me ! {done, Resp}
    end,
    spawn(fun() -> Req(1) end),
    spawn(fun() -> Req(2) end),
    spawn(fun() ->
        dummy_worker:spindown(resolve(1))
    end),
    spawn(fun() ->
        dummy_worker:spindown(resolve(2))
    end),
    receive
        {done, ok} -> ok
    end,
    receive
        {done, ok} -> ok
    end,
    stop_manager(),
    ok.

shutdown_spindown_test() ->
    start_manager(),
    Key = 1,
    ondemand_manager:ensure_started(manager, Key),
    Ref = monitor(process, resolve(Key)),
    Me = self(),
    Req = fun() ->
        Resp = request(Key),
        Me ! {done, Resp}
    end,
    spawn(Req),
    spawn(Req),
    spawn(fun() ->
        dummy_worker:spindown_with(resolve(Key), {shutdown, spindown})
    end),
    receive
        {'DOWN', Ref, process, _, _} ->
            ok
    end,
    receive
        {done, ok} -> ok;
        {done, died} -> ok
    end,
    receive
        {done, ok} -> ok;
        {done, died} -> ok
    end,
    stop_manager(),
    ok.
