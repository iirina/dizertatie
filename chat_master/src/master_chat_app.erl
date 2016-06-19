-module(master_chat_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    logger:debug("master_chat_app:start()"),
    case master_chat_supervisor:start_link() of
        {ok, Pid} ->
            logger:debug("master_chat_app:start(): master_chat_supervisor started"),
            {ok, Pid};
        Other ->
            logger:debug("master_chat_app:start(): master_chat_supervisor DID NOT start due to ~w", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
