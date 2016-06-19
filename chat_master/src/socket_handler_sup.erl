%% Supervises the socket_handler process.

-module(socket_handler_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    logger:debug("socket_handler_sup:start_link() Will start initializing."),
    supervisor:start_link({local, socket_handler_sup}, socket_handler_sup, []).

init(Args) ->
    logger:debug("socket_handler_sup:init() with arguments ~p", [Args]),
    SocketHandlerSpec = {
        socket_handler,
        {socket_handler, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [socket_handler]
    },
    SupFlags = {simple_one_for_one, 0, 1},
    logger:debug("socket_handler_sup:init() finished."),
    {ok, {SupFlags, [SocketHandlerSpec]}}.
