%% This module is the root of this chat supervisor tree. It directly supervises:
%% (*) The courier
%% (*) The socket acceptor supervisor
%% (*) The socket handler supervisor
%%
%% All supervised procceses are permanent.

-module(chat_supervisor).
-behaviour(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    logger:debug("chat_supervisor:start_link()"),
    supervisor:start_link({local, chat_supervisor}, chat_supervisor, []).

init(_Args) ->
    logger:debug("chat_supervisor:init() PID = ~w", [self()]),

    SocketAcceptorSupSpec = {
        socket_acceptor_sup,
        {socket_acceptor_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [socket_acceptor_sup]
    },

    SocketHandlerSupSpec = {
        socket_handler_sup,
        {socket_handler_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [socket_handler_sup]
    },

    CourierSpec = {
        courier,
        {courier, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [courier]
    },

    RosterSpec = {
        roster,
        {roster, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [roster]
    },

    ChildSpec = [RosterSpec, CourierSpec, SocketHandlerSupSpec, SocketAcceptorSupSpec],
    SupFlags = {one_for_one, 10, 1},
    {ok, {SupFlags, ChildSpec}}.
