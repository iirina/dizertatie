%% Supervises the socket_acceptor_sup

-module(socket_acceptor_sup).
-behaviour(supervisor_bridge).

%% API
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([
    init/1,
    terminate/2
]).

start_link() ->
    supervisor_bridge:start_link({local, socket_acceptor_sup}, socket_acceptor_sup, []).

init(_Args) ->
    logger:debug("socket_acceptor_sup:init() PID is ~p", [self()]),
    {ok, SocketAcceptorPid} = socket_acceptor:start(),
    {ok, SocketAcceptorPid, SocketAcceptorPid}.

terminate(Reason, State) ->
    SocketAcceptorPid = State,
    logger:debug("socket_acceptor_sup:terminate() Reason is ~w", [Reason]),
    exit(SocketAcceptorPid, Reason).
