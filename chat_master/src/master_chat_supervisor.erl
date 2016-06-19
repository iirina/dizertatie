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

-include("./macros.hrl").

start_link() ->
    logger:debug("chat_supervisor:start_link()"),
    supervisor:start_link({local, chat_supervisor}, chat_supervisor, []).

mysql_init() ->
    p1_mysql:start_link(?MYSQL_ID, ?MYSQL_HOST, ?MYSQL_USER, ?MYSQL_PASSWORD, ?MYSQL_DATABASE,
        fun(_Level, Format, Args) -> logger:debug(Format, Args) end),
    timer:sleep(2000).

init_db() ->
    case ?STORAGE of
        ?MYSQL ->
            mysql_init();
        ?MNESIA ->
            % mnesia_utils:mnesia_init()
            ok
    end.

init(_Args) ->
    logger:debug("chat_supervisor:init() PID = ~w", [self()]),

    %% Starting mnesia OR mysql.
    init_db(),

    ConnDistrSupSpec = {
        conn_distr_sup,
        {conn_distr_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [conn_distr_sup]
    },

    CourierSpec = {
        courier,
        {courier, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [courier]
    },

    RosterSupSpec = {
        roster_sup,
        {roster_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [roster_sup]
    },

    RosterMasterSpec = {
        roster_master,
        {roster_master, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [roster_master]
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
        RosterSupSpec,
        RosterMasterSpec,
        CourierSpec,
        ConnDistrSupSpec
    ],
    SupFlags = {one_for_one, 10, 1},
    {ok, {SupFlags, ChildSpec}}.
