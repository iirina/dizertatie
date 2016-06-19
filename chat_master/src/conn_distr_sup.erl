%% Supervises conn_distr

-module(conn_distr_sup).
-behaviour(supervisor_bridge).

%% API
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([
    init/1,
    terminate/2
]).

start_link() ->
    supervisor_bridge:start_link({local, conn_distr_sup}, conn_distr_sup, []).

init(_Args) ->
    logger:debug("conn_distr_sup:init() PID is ~p", [self()]),
    {ok, ConnDistrPid} = conn_distr:start(),
    {ok, ConnDistrPid, ConnDistrPid}.

terminate(Reason, State) ->
    ConnDistrPid = State,
    logger:debug("conn_distr_sup:terminate() Reason is ~w", [Reason]),
    exit(ConnDistrPid, Reason).
