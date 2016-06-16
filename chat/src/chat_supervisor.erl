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

-define(MYSQL_ID, "1234").
-define(MYSQL_HOST, "localhost").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWORD, "parola").
-define(MYSQL_DATABASE, "chat").


start_link() ->
    logger:debug("chat_supervisor:start_link()"),
    supervisor:start_link({local, chat_supervisor}, chat_supervisor, []).

init(_Args) ->
    logger:debug("chat_supervisor:init() PID = ~w", [self()]),

    %% Starting mnesia and mysql.
    mnesia:start(),
    p1_mysql:start_link(?MYSQL_ID, ?MYSQL_HOST, ?MYSQL_USER, ?MYSQL_PASSWORD, ?MYSQL_DATABASE,
        fun(_Level, Format, Args) -> logger:debug(Format, Args) end),

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

    RegistrationSpec = {
        registration,
        {registration, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [registration]
    },

    ChildSpec = [
        RegistrationSpec,
        RosterSpec,
        CourierSpec,
        SocketHandlerSupSpec,
        SocketAcceptorSupSpec
    ],
    SupFlags = {one_for_one, 10, 1},
    {ok, {SupFlags, ChildSpec}}.
