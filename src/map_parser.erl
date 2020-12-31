-module(map_parser).
-export([parse/1]).
%TODO: add tests

parse(MapString) ->
    Numbered = sub_numbers(MapString),
    break_map(Numbered).

break_map(MapString) ->
    string:split(MapString, "/", all).

numbers_to_spaces(Number) ->
    Numberized = list_to_integer(Number),
    string:copies(" ", Numberized).

%% guard means that Char is a number
sub_numbers([]) ->
    [];
sub_numbers([Head | Rest]) when (Head >= 48 andalso Head =< 57) ->
    Second = lists:nth(1, Rest),
    %TODO: check if there is better lists handlind method
    case Second >= 48 andalso Second =< 57 of
        true -> [numbers_to_spaces([Head, Second])] ++ sub_numbers(Rest -- [Second]);
        false -> [numbers_to_spaces([Head]), Second] ++ sub_numbers(Rest -- [Second])
    end;
sub_numbers([Head | Rest]) ->
    [Head] ++ sub_numbers(Rest).

