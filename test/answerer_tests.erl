-module(answerer_tests).

-include_lib("eunit/include/eunit.hrl").

answerer_test_() ->
    [answer_interface_correct(),
     answer_interface_wrong(""),
     answer_interface_wrong("Otherwise"),
     answer_version_correct(),
     answer_version_wrong(""),
     answer_version_wrong("Otherwise"),
     answer_color_blue(),
     answer_color_red()].

answer_interface_correct() ->
    ?_assertEqual("tbi ok\n", answerer:answer_interface("tbi\n")).

answer_interface_wrong(Message) ->
    ?_assertException(error, function_clause, answerer:answer_interface(Message)).

answer_version_correct() ->
    ?_assertEqual("tbi v1 ok\n", answerer:answer_version("tbi v1\n")).

answer_version_wrong(Message) ->
    ?_assertException(error, function_clause, answerer:answer_version(Message)).

answer_color_blue() ->
    ?_assertEqual({ok, blue}, answerer:answer_color("color blue\n")).

answer_color_red() ->
    ?_assertEqual({ok, red}, answerer:answer_color("color red\n")).
