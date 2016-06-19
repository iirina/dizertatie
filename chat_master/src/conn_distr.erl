%% Accepts socket connections and spawns a new proccess after each new connection established.

-module(conn_distr).

-export([start/0]).

-export([
    init/1,
    send_addr_to_socket/1
]).

-define(DEFAULT_PORT, 5455).

start() ->
    pool:start(),
    SocketOpts = [{active, false}, binary, {packet, 0}],
    Port = ?DEFAULT_PORT,
    case gen_tcp:listen(Port, SocketOpts) of
        {ok, ListenSocket} ->
            logger:info("Server listening on port ~p", [Port]),
            Pid = spawn_link(conn_distr, init, [ListenSocket]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

init(ListenSocket) ->
    logger:debug("conn_distr:init() Will initialize loop."),
    loop(ListenSocket).

get_addr() ->
    % TODO: use OTP pool to retrieve node
    "Connect to localhost:5455\n".

send_addr_to_socket(Socket) ->
    Addr = get_addr(),
    case gen_tcp:send(Socket, Addr) of
        ok ->
            logger:debug("conn_distr:send_addr_to_socket(~p) ~p", [Socket, Addr]);
        {error, Reason} ->
            logger:debug("conn_distr:send_addr_to_socket(~p) ~p not sent due to reason ~p",
            [Socket, Addr, Reason])
    end,
    gen_tcp:close(Socket).

loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            logger:debug(
                "conn_distr:loop() Accepted connection with socket ~p", [AcceptSocket]),
            spawn_link(conn_distr, send_addr_to_socket, [AcceptSocket]),
            loop(ListenSocket);
        {error, Reason} ->
            logger:error("conn_distr:loop() Stopping for reason ~p", [Reason]),
            exit(Reason)
    end.
