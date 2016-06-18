-record(user, {username, password}).

-record(friends, {user, friend}).

-record(courier_user_to_pid, {username, pid}).

-export([
    mnesia_init/0,
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

mnesia_init() ->
    ok.
    % mnesia:start(),
    % mnesia:create_table(user,
    %                     [{attributes, record_info(fields, user)}]),
    % mnesia:create_table(friends,
    %                     [{attributes, record_info(fields, friends)}, {type, bag}]),
    %
    % mnesia:create_table(courier_user_to_pid,
    %                     [{attributes, record_info(fields, courier_user_to_pid)}]).

insert_user(Username, Passowrd) ->
    % insert_object(#user{username = Username, password = Passowrd}).
    ok.

insert_friendship(User, Friend) ->
    % insert_object(#friends{user = User, friend = Friend}).
    ok.

insert_to_courier(User, Pid) ->
    % insert_object(#courier_user_to_pid{username = User, pid = Pid}).
    ok.

get_pid_for_user(User) ->
    % case mnesia:dirty_read(courier_user_to_pid, User) of
    %     {atomic, [{courier_user_to_pid, _User, Pid}]} ->
    %         Pid;
    %     _Other ->
    %         no_pid
    % end.
    no_pid.

remove_from_courier(User, Pid) ->
    % mnesia:dirty_delete_object({courier_user_to_pid, User, Pid}).
    ok.

get_user(Username) ->
    % mnesia:dirty_read(user, Username).
    ok.

get_friends_for_user(Username) ->
    % mnesia:dirty_read(friends, Username).
    [].

get_all_users() ->
    % User = #user{username='$1',password='$2'},
    % mnesia:dirty_select(user, [{User, [], [['$1', '$2']]}]).
    [].

are_mnesia_friends(U1, U2) ->
    % case mnesia:dirty_match_object({friends, U1, U2}) of
    %     {atomic, [{friends, _User1, _User2}]} ->
    %         true;
    %     _Other ->
    %         false
    % end.
    true.

insert_object(Object) ->
    mnesia:dirty_write(Object).
