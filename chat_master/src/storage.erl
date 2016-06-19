-module(storage).

-export([
    load_users/0
]).

-include("macros.hrl").

load_users() ->
    case ?STORAGE of
        ?MNESIA ->
            mnesia_utils:get_all_users();
        ?MYSQL ->
            mysql_utils:get_all_users()
    end.
