-module(chat_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    logger:debug("chat_app:start()"),
    case chat_supervisor:start_link() of
        {ok, Pid} ->
            logger:debug("chat_app:start(): chat_supervisor started"),
            {ok, Pid};
        Other ->
            logger:debug("chat_app:start(): chat_supervisor DID NOT start due to ~w", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
