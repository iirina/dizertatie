%% This module is the root of this chat supervisor tree. It directly supervises:
%% (*) The master_courier
%% (*) The socket acceptor supervisor
%% (*) The socket handler supervisor
%%
%% All supervised procceses are permanent.

-module(master_chat_supervisor).
-behaviour(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("./macros.hrl").

start_link() ->
    logger:debug("master_chat_supervisor:start_link()"),
    supervisor:start_link({local, master_chat_supervisor}, master_chat_supervisor, []).

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
    logger:debug("master_chat_supervisor:init() PID = ~w", [self()]),

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
        master_courier,
        {master_courier, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [master_courier]
    },

    RosterSupSpec = {
        master_roster_sup,
        {master_roster_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [master_roster_sup]
    },

    RosterMasterSpec = {
        master_roster_master,
        {master_roster_master, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [master_roster_master]
    },

    RegistrationSpec = {
        master_registration,
        {master_registration, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [master_registration]
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
