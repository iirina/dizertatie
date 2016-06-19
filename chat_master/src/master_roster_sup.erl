%% Supervises the roster processes (simple_one_for_one).

-module(master_roster_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    logger:debug("master_roster_sup:start_link() Will start initializing."),
    supervisor:start_link({local, master_roster_sup}, master_roster_sup, []).

init(Args) ->
    logger:debug("master_roster_sup:init() with arguments ~p", [Args]),
    RosterSpec = {
        master_roster,
        {master_roster, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [master_roster]
    },
    SupFlags = {simple_one_for_one, 0, 1},
    logger:debug("master_roster_sup:init() finished."),
    {ok, {SupFlags, [RosterSpec]}}.
