-module(chat_utils).

-export([
    format_notification/2,
    format_message/3,
    log_message/3,
    log_message/2,
    string_timestamp/0,
    trim_string/1,
    string_starts_with/2
]).


trim_string(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

string_starts_with(String, StartsWith) ->
    FindIndex = string:str(String, StartsWith),
    FindIndex == 1.

%% makes a notification of joining, leaving, etc.
format_notification(Name, Action) ->
	string_timestamp() ++ " " ++ Name ++ " " ++ Action ++ ".".

%% formats a received message that the user typed. Msg is String.
format_message(Name, Msg, MessagingType) ->
	string_timestamp() ++ " " ++ Name ++ " (" ++ MessagingType ++ ")> " ++ Msg.

%% saves a log entry: <client address> <message>
log_message(Name, Format, Args) ->
	logger:info(Name ++ " " ++ Format, Args).

log_message(Name, String) ->
	logger:info(Name ++ " " ++ String).

add_leading_zero(Number) ->
    if
        Number < 10  -> "0" ++ integer_to_list(Number);
        Number >= 10 -> integer_to_list(Number)
    end.

string_timestamp() ->
    {{_Year, _Month, _Day}, {Hour,Min,Sec}} = erlang:localtime(),
    add_leading_zero(Hour) ++ ":" ++ add_leading_zero(Min) ++ ":" ++ add_leading_zero(Sec).
