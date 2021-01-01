-module(map_parser_tests).

-include_lib("eunit/include/eunit.hrl").

map_parser_test_() ->
    [parse_null_map(),
     parse_small_map(),
     parse_medium_map(),
     parse_large_map()].

parse_null_map() ->
    ?_assertEqual(test_utility:test_map_null(), map_parser:parse(test_utility:test_map_string_null())).

parse_small_map() ->
    ?_assertEqual(test_utility:test_map_small(), map_parser:parse(test_utility:test_map_string_small())).

parse_medium_map() ->
    ?_assertEqual(test_utility:test_map_medium(), map_parser:parse(test_utility:test_map_string_medium())).

parse_large_map() ->
    ?_assertEqual(test_utility:test_map_large(), map_parser:parse(test_utility:test_map_string_large())).
