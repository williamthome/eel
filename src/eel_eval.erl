%%%-----------------------------------------------------------------------------
%%% @doc EEl evaluator module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_eval).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    binary/2,
    binary/3,
    compiled/2,
    compiled/3
]).

-export_type([
    return/0
]).

-type return() :: binary().

%%------------------------------------------------------------------------------
%% @doc Evaluates a binary.
%% @end
%%------------------------------------------------------------------------------
-spec binary(binary(), eel_render:bindings()) -> return().

binary(Bin, Bindings) ->
    binary(Bin, #{}, Bindings).

%%------------------------------------------------------------------------------
%% @doc Evaluates a binary passing a memo.
%% @end
%%------------------------------------------------------------------------------
-spec binary(binary(), eel_render:memo(), eel_render:bindings()) -> return().

binary(Bin, Memo, Bindings) when is_binary(Bin) ->
    Compiled = eel_compile:binary(Bin),
    compiled(Compiled, Memo, Bindings).

%%------------------------------------------------------------------------------
%% @doc Evaluates.
%% @end
%%------------------------------------------------------------------------------
-spec compiled(eel_compile:return(), eel_render:bindings()) -> return().

compiled(Compiled, Bindings) ->
    compiled(Compiled, #{}, Bindings).

%%------------------------------------------------------------------------------
%% @doc Evaluates passing a memo.
%% @end
%%------------------------------------------------------------------------------
-spec compiled(eel_compile:return(), eel_render:memo(), eel_render:bindings()) -> return().

compiled(Compiled, Memo, Bindings) ->
    {Rendered, _, _} = eel_render:compiled(Compiled, Memo, Bindings),
    Rendered.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

binary_test() ->
    ?assertEqual(
        <<"Hello, World!">>,
        binary(<<"Hello, <%= Name .%>!">>, #{'Name' => <<"World">>})
    ).

-endif.
