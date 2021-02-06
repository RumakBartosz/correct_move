-module(map_parser).

-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec parse(string()) -> type:tron_map().

parse(MapString) ->
    Numbered = sub_numbers(MapString),
    break_map(Numbered).

%%====================================================================
%% Internal functions
%%====================================================================

-spec break_map(string()) -> [string()].

break_map(MapString) ->
    string:split(MapString, "/", all).

-spec numbers_to_spaces(nonempty_string()) -> string().

numbers_to_spaces(Number) ->
    Numberized = list_to_integer(Number),
    string:copies(" ", Numberized).

-spec sub_numbers(string()) -> string().

sub_numbers("") ->
    "";
sub_numbers([Head, Second | Rest]) when (Head >= $0 andalso Head =< $9) ->
    case Second >= 48 andalso Second =< 57 of
        true -> [numbers_to_spaces([Head, Second])] ++ sub_numbers(Rest);
        false -> [numbers_to_spaces([Head]), Second] ++ sub_numbers(Rest)
    end;
sub_numbers([Head | Rest]) ->
    [Head | sub_numbers(Rest)].

