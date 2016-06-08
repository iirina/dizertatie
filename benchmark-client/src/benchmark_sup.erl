%% This module is the root of this chat supervisor tree.
%%
%% All supervised procceses are permanent.

-module(benchmark_sup).
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
    logger:debug("benchmark_sup:start_link()"),
    supervisor:start_link({local, benchmark_sup}, benchmark_sup, []).

init(_Args) ->
    logger:debug("benchmark_sup:init() PID = ~w", [self()]),

    p1_mysql:start_link(?MYSQL_ID, ?MYSQL_HOST, ?MYSQL_USER, ?MYSQL_PASSWORD, ?MYSQL_DATABASE,
        fun(_Level, Format, Args) -> logger:debug(Format, Args) end),

    BenchmarkGeneratorSup = {
        bm_generator_sup,
        {bm_generator_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [bm_generator_sup]
    },

    ChatClientSup = {
        chat_client_sup,
        {chat_client_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [chat_client_sup]
    },

    Generator = {
        generator,
        {generator, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [generator]
    },

    ChildSpec = [
        Generator,
        ChatClientSup,
        BenchmarkGeneratorSup
    ],
    SupFlags = {one_for_one, 10, 1},
    {ok, {SupFlags, ChildSpec}}.
