%% Supervises the roster processes (simple_one_for_one).

-module(roster_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    logger:debug("roster_sup:start_link() Will start initializing."),
    supervisor:start_link({local, roster_sup}, roster_sup, []).

init(Args) ->
    logger:debug("roster_sup:init() with arguments ~p", [Args]),
    RosterSpec = {
        roster,
        {roster, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [roster]
    },
    SupFlags = {simple_one_for_one, 0, 1},
    logger:debug("roster_sup:init() finished."),
    {ok, {SupFlags, [RosterSpec]}}.
