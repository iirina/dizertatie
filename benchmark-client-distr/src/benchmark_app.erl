-module(benchmark_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    logger:debug("benchmark_app:start()"),
    case benchmark_sup:start_link() of
        {ok, Pid} ->
            logger:debug("benchmark_app:start(): benchmark_sup started"),
            {ok, Pid};
        Other ->
            logger:debug("benchmark_app:start(): benchmark_sup DID NOT start due to ~w", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
