-module(answerer).

-export([answer_interface/1,
         answer_version/1,
         answer_color/1,
         answer_move/2]).


%%====================================================================
%% API functions
%%====================================================================

answer_interface("tbi\n") ->
    "tbi ok\n".

answer_version("tbi v1\n") ->
    "tbi v1 ok\n".

%TODO: change useless tuple returns
%TODO: add tests
%TODO: work on atoms instead of strings
answer_color("color blue\n") ->
    {ok, blue};
answer_color("color red\n") ->
    {ok, red}.

answer_move(Color, Map) ->
    rand_bot:choose_move(Color, Map).
