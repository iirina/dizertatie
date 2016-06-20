%% Accepts socket connections and spawns a new proccess after each new connection established.

-module(socket_acceptor).

-export([start/0]).

-export([init/1]).

-include("macros.hrl").

start() ->
    SocketOpts = [{active, false}, binary, {packet, 0}],
    Port = ?DEFAULT_SLAVE_PORT,
    case gen_tcp:listen(Port, SocketOpts) of
        {ok, ListenSocket} ->
            logger:info("Server listening on port ~p", [Port]),
            Pid = spawn_link(socket_acceptor, init, [ListenSocket]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

init(ListenSocket) ->
    logger:debug("socket_acceptor:init() Will initialize loop."),
    loop(ListenSocket).

loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            logger:debug(
                "socket_acceptor:loop() Accepted connection with socket ~p", [AcceptSocket]),
            socket_handler:start(AcceptSocket),
            loop(ListenSocket);
        {error, Reason} ->
            logger:error("socket_acceptor:loop() Stopping for reason ~p", [Reason]),
            exit(Reason)
    end.
