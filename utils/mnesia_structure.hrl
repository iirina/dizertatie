-record(user, {username, password}).

-record(friends, {user, friend}).

-record(courier_pid_to_user, {pid, username}).

-record(courier_user_to_pid, {username, pid}).

-export([
    insert_user/2,
    insert_friendship/2,
    insert_to_courier/2,
    get_all_users/0,
    get_user/1,
    get_friends_for_user/1,
    are_mnesia_friends/2,
    get_pid_for_user/1,
    remove_from_courier/2
]).

insert_user(Username, Passowrd) ->
    insert_object(#user{username = Username, password = Passowrd}).

insert_friendship(User, Friend) ->
    insert_object(#friends{user = User, friend = Friend}).

insert_to_courier(User, Pid) ->
    insert_object(#courier_user_to_pid{username = User, pid = Pid}).

get_pid_for_user(User) ->
    F = fun() ->
        mnesia:read(courier_user_to_pid, User)
    end,
    case mnesia:transaction(F) of
        {atomic, [{courier_user_to_pid, _User, Pid}]} ->
            Pid;
        _Other ->
            no_pid
    end.

remove_from_courier(User, Pid) ->
    F = fun() ->
            mnesia:delete_object({courier_user_to_pid, User, Pid})
        end,
    mnesia:transaction(F).

get_user(Username) ->
    F = fun() ->
        mnesia:read(user, Username)
    end,
    mnesia:transaction(F).

get_friends_for_user(Username) ->
    F = fun() ->
        mnesia:read(friends, Username)
    end,
    mnesia:transaction(F).

get_all_users() ->
    F = fun() ->
		User = #user{username='$1',password='$2'},
		mnesia:select(user, [{User, [], [['$1', '$2']]}])
        end,
    mnesia:transaction(F).

are_mnesia_friends(U1, U2) ->
    F = fun() ->
    		mnesia:match_object({friends, U1, U2})
        end,
    case mnesia:transaction(F) of
        {atomic, [{friends, _User1, _User2}]} ->
            true;
        _Other ->
            false
    end.

insert_object(Object) ->
    Fun = fun() ->
                  mnesia:write(Object)
          end,
    mnesia:transaction(Fun).
