-module(eel_tokenizer).

%% API functions
-export([tokenize/1, tokenize/2,
         compile/1, compile/2,
         expr_to_ast/1,
         normalize_expr/1,
         merge_sd/1, merge_sd/2,
         render/1, render/2]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([markers/0,
         init/1,
         handle_expr/4, handle_text/3, handle_body/1]).
-endif.

%% Defines
-define(DEFAULT_ENGINE, eel_smart_engine).
-define(DEFAULT_ENGINE_OPTS, #{}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc tokenize/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary()) -> list().

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary(), map()) -> list().

tokenize(Bin, Opts) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    Pos = {1, 1},
    do_tokenize(Bin, Pos, Pos, <<>>, Eng, State).

%% -----------------------------------------------------------------------------
%% @doc compile/1.
%% @end
%% -----------------------------------------------------------------------------
-spec compile({[binary()], list()}) -> list().

compile(Tokens) ->
    compile(Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile({[binary()], list()}, map()) -> list().

compile({Static, Dynamic}, Opts) when is_list(Static), is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    DAST = do_compile(Dynamic, Eng, State),
    SAST = lists:map(fun(S) -> expr_to_ast(["<<\"", S, "\">>"]) end, Static),
    merge_sd(SAST, DAST).

%% -----------------------------------------------------------------------------
%% @doc expr_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec expr_to_ast(binary() | string()) -> list().

expr_to_ast(Expr) ->
    {ok, Tokens, _} = erl_scan:string(normalize_expr(Expr)),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.

%% -----------------------------------------------------------------------------
%% @doc normalize_expr/1.
%% @end
%% -----------------------------------------------------------------------------
-spec normalize_expr(binary() | string()) -> string().

% TODO: Check if expr has dot at the end
normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/1.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd({[binary()], list()}) -> list().

merge_sd({Static, Dynamic}) ->
    merge_sd(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/2.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd([binary()], list()) -> list().

merge_sd(Static, Dynamic) ->
    do_merge_sd(Static, Dynamic, []).

%% -----------------------------------------------------------------------------
%% @doc render/1.
%% @end
%% -----------------------------------------------------------------------------
-spec render(list()) -> binary().

render(AST) ->
    render(AST, #{}).

%% -----------------------------------------------------------------------------
%% @doc render/2.
%% @end
%% -----------------------------------------------------------------------------
-spec render(list(), map() | proplists:proplist()) -> binary().

render(ASTList, Bindings) ->
    erlang:iolist_to_binary(lists:map(fun(AST) -> eval(AST, Bindings) end, ASTList)).

eval(AST, Bindings) ->
    % FIXME: New bindings must be available to nested expressions
    {value, Binary, _NewBindings} = erl_eval:exprs(AST, Bindings),
    Binary.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_tokenize(<<>>, _, Pos, Text, Eng, State) ->
    StateEOF = case Text =:= <<>> of
                   true -> State;
                   false -> Eng:handle_text(Pos, Text, State)
               end,
    Eng:handle_body(StateEOF);
do_tokenize(Bin, OldPos, CurPos, Text, Eng, State) ->
    case retrieve_marker(Eng:markers(), Bin) of
        {true, {Marker, Expr, BinRest}} ->
            StateText = case Text =:= <<>> of
                            true -> State;
                            false -> Eng:handle_text(OldPos, Text, State)
                        end,
            StateExpr = Eng:handle_expr(CurPos, Marker, Expr, StateText),
            NewPos = expr_position(Expr, Marker, CurPos),
            do_tokenize(BinRest, NewPos, NewPos, <<>>, Eng, StateExpr);
        false ->
            {BinRest, NewPos, TextAcc} = do_text_acc(Bin, OldPos, Text),
            do_tokenize(BinRest, OldPos, NewPos, TextAcc, Eng, State)
    end.

retrieve_marker(EngMarkers, Bin) ->
    case match_markers_start(EngMarkers, Bin) of
        [] -> false;
        Markers -> case best_match(match_markers_end(Markers), undefined) of
                       undefined -> error(end_marker_not_found);
                       Marker -> {true, Marker}
                   end
    end.

match_markers_start(_, <<>>) ->
    [];
match_markers_start(Markers, Bin) ->
    lists:filtermap(
        fun({StartMarker, _} = Marker) ->
            StartMarkerLength = length(StartMarker),
            case size(Bin) >= StartMarkerLength andalso
                 binary:split(Bin,
                              list_to_binary(StartMarker),
                              [{scope, {0, StartMarkerLength}}])
            of
                [<<>>, Rest] -> {true, {Marker, Rest}};
                _ -> false
            end
        end,
        Markers
    ).

match_markers_end(Markers) ->
    lists:filtermap(
        fun({{_, EndMarker} = Marker, Bin}) ->
            case size(Bin) >= length(EndMarker) andalso
                 binary:split(Bin, list_to_binary(EndMarker))
            of
                [Expr, Rest] -> {true, {Marker, string:trim(Expr), Rest}};
                _ -> false
            end
        end,
        Markers
    ).

best_match([Best | Markers], undefined) ->
    best_match(Markers, Best);
best_match([{_, Expr, _} = Best | Markers], {_, BestExpr, _})
    when size(Expr) < size(BestExpr)
->
    best_match(Markers, Best);
best_match([_ | Markers], Best) ->
    best_match(Markers, Best);
best_match([], Best) ->
    Best.

do_text_acc(<<"\n", T/binary>>, {Ln, _}, Text) ->
    {T, {Ln + 1, 1}, Text};
do_text_acc(<<H, T/binary>>, {Ln, Col}, Text) ->
    {T, {Ln, Col + 1}, <<Text/binary, H>>};
do_text_acc(<<>>, Pos, Text) ->
    {<<>>, Pos, Text}.

expr_position(Expr, {StartMarker, EndMarker}, OldPos) ->
    Pos = add_marker_length(StartMarker, OldPos),
    do_expr_position(Expr, EndMarker, Pos).

do_expr_position(<<"\n", T/binary>>, EndMarker, {Ln, _}) ->
    do_expr_position(T, EndMarker, {Ln + 1, 1});
do_expr_position(<<_, T/binary>>, EndMarker, {Ln, Col}) ->
    do_expr_position(T, EndMarker, {Ln, Col + 1});
do_expr_position(<<>>, EndMarker, Pos) ->
    add_marker_length(EndMarker, Pos).

add_marker_length(Marker, {Ln, Col}) ->
    {Ln, Col + length(Marker)}.

do_compile([D | Dynamic], Eng, State) ->
    NewState = Eng:handle_compile(D, State),
    do_compile(Dynamic, Eng, NewState);
do_compile([], Eng, State) ->
    Eng:handle_ast(State).

do_merge_sd([S | Static], [D | Dynamic], Acc) ->
    do_merge_sd(Static, Dynamic, [D, S | Acc]);
do_merge_sd([S | Static], [], Acc) ->
    do_merge_sd(Static, [], [S | Acc]);
do_merge_sd([], [], Acc) ->
    lists:reverse(Acc);
do_merge_sd([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% TODO: Test position (check if trims are causing issues).
tokenize_test() ->
    Bin = <<"Hello,\n{{ World }}!">>,
    Expected = [
        {text, {{1, 1}, <<"Hello,">>}},
        {expr, {{2, 1}, {"{{", "}}"}, <<"World">>}},
        {text, {{2, 11}, <<"!">>}}
    ],
    ?assertEqual(Expected, tokenize(Bin, #{engine => ?MODULE})).

% Engine

markers() ->
    [{"{{", "}}"}].

init(#{}) ->
    [].

handle_expr({Ln, Col}, {StartMarker, EndMarker}, Expr, Acc) ->
    [{expr, {{Ln, Col}, {StartMarker, EndMarker}, Expr}} | Acc].

handle_text({Ln, Col}, Text, Acc) ->
    [{text, {{Ln, Col}, Text}} | Acc].

handle_body(Tokens) ->
    lists:reverse(Tokens).

-endif.
