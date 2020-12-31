-module(answerer).

-export([answer_interface/1,
         answer_version/1,
         answer_color/1,
         answer_move/2]).


answer_interface("tbi\n") ->
    "tbi ok\n".

answer_version("tbi v1\n") ->
    "tbi v1 ok\n".

%TODO: change useless tuple returns
%TODO: add tests
%TODO: work on atoms instead of strings
answer_color("color blue\n") ->
    {"blue", "color ok"};
answer_color("color red\n") ->
    {"red", "color ok"}.

answer_move(Color, Map) ->
    rand_bot:choose_move(Color, Map).
