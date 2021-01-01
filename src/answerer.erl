-module(answerer).

-export([answer_interface/1,
         answer_version/1,
         answer_color/1,
         answer_move/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec answer_interface(type:message()) -> type:message().

answer_interface("tbi\n") ->
    "tbi ok\n".

-spec answer_version(type:message()) -> type:message().

answer_version("tbi v1\n") ->
    "tbi v1 ok\n".

-spec answer_color(type:message()) -> {ok, type:color()}.

answer_color("color blue\n") ->
    {ok, blue};
answer_color("color red\n") ->
    {ok, red}.

-spec answer_move(type:color(), type:tron_map()) -> type:move().

answer_move(Color, Map) ->
    rand_bot:choose_move(Color, Map).
