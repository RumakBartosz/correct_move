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
    ?_assertEqual("tbi ok", answerer:answer_interface("tbi")).

answer_interface_wrong(Message) ->
    ?_assertException(error, function_clause, answerer:answer_interface(Message)).

answer_version_correct() ->
    ?_assertEqual("tbi v1 ok", answerer:answer_version("tbi v1")).

answer_version_wrong(Message) ->
    ?_assertException(error, function_clause, answerer:answer_version(Message)).

answer_color_blue() ->
    ?_assertEqual({blue, "color ok"}, answerer:answer_color("color blue")).

answer_color_red() ->
    ?_assertEqual({red, "color ok"}, answerer:answer_color("color red")).
