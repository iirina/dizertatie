-record(user, {
                username,
                password
        }).

-record(friends, {
                    user,
                    friend
        }).

-record(benchmark, {
                        id,
                        type,
                        timestamp,
                        msg,
                        req_type
                    }).

-export([
    insert_user/2,
    insert_friendship/2,
    insert_benchmark/5,
    get_all_users/0
]).

insert_user(Username, Passowrd) ->
    insert_object(#user{username = Username, password = Passowrd}).

insert_friendship(User, Friend) ->
    insert_object(#friends{user = User, friend = Friend}).

insert_benchmark(Id, Type, Timestamp, Msg, ReqType) ->
    insert_object(#benchmark{id = Id, type = Type, timestamp = Timestamp, msg = Msg, req_type = ReqType}).

get_all_users() ->
    F = fun() ->
		User = #user{username='$1',password='$2'},
		mnesia:select(user, [{User, [], [['$1', '$2']]}])
        end,
    mnesia:transaction(F).

insert_object(Object) ->
    Fun = fun() ->
                  mnesia:write(Object)
          end,
    mnesia:transaction(Fun).
