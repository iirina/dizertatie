%% Supervises the chat_client process.

-module(chat_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    logger:debug("chat_client_sup:start_link() Will start initializing."),
    supervisor:start_link({local, chat_client_sup}, chat_client_sup, []).

init(Args) ->
    logger:debug("chat_client_sup:init() with arguments ~p", [Args]),
    ChatClientSpec = {
        chat_client,
        {chat_client, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [chat_client]
    },
    SupFlags = {simple_one_for_one, 0, 1},
    logger:debug("chat_client_sup:init() finished."),
    {ok, {SupFlags, [ChatClientSpec]}}.
