%% Supervises the bm_generator_sup

-module(bm_generator_sup).
-behaviour(supervisor_bridge).

%% API
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([
    init/1,
    terminate/2
]).

start_link() ->
    supervisor_bridge:start_link({local, bm_generator_sup}, bm_generator_sup, []).

init(_Args) ->
    logger:debug("bm_generator_sup:init() PID is ~p", [self()]),
    {ok, BmGeneratorPid} = bm_generator:start_link(),
    {ok, BmGeneratorPid, BmGeneratorPid}.

terminate(Reason, State) ->
    BmGeneratorPid = State,
    logger:debug("bm_generator_sup:terminate() Reason is ~w", [Reason]),
    exit(BmGeneratorPid, Reason).
