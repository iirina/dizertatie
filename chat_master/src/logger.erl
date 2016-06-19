-module(logger).

-export([
    info/1,
    info/2,
    debug/1,
    debug/2,
    error/1,
    error/2
]).

-define(INFO, "INFO").
-define(DEBUG, "DEBUG").
-define(ERROR, "ERROR").

%%%===================================================================
%%% API
%%%===================================================================
info(String) ->
    print(?INFO, String).

info(Format, Args) ->
    print(?INFO, Format, Args).

debug(String) ->
    print(?DEBUG, String).

debug(Format, Args) ->
    print(?DEBUG, Format, Args).

error(String) ->
    print(?ERROR, String).

error(Format, Args) ->
    print(?ERROR, Format, Args).

%%%===================================================================
%%% Helper functions
%%%===================================================================
print(Level, String) ->
    io:fwrite("~s [~s] ~p ~p~n", [chat_utils:string_timestamp(), Level, self(), String]).

print(Level, Format, Args) ->
    Pid = pid_to_list(self()),
    io:fwrite(chat_utils:string_timestamp() ++ " [" ++ Level ++ "] " ++ Pid ++ " " ++ Format ++ "\n", Args).
