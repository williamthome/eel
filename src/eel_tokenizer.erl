%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl tokenizer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_tokenizer).

%% API functions
-export([tokenize/1, tokenize/2,
         tokenize_file/1, tokenize_file/2]).

-ifdef(TEST).
%% Test functions
-export([markers/0,
         init/1,
         handle_expr/4, handle_text/3, handle_body/1]).
-endif.

%% Types
-export_type([dynamic/0, tokens/0]).

%% Includes
-include("eel.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type dynamic() :: term().
-type tokens() :: {eel_engine:static(), dynamic()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc tokenize/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary()) -> tokens().

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary(), map()) -> tokens().

tokenize(Bin, Opts) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    Pos = {1, 1},
    do_tokenize(Bin, Pos, Pos, <<>>, Eng, State).

%% -----------------------------------------------------------------------------
%% @doc tokenize_file/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize_file(file:filename_all()) -> tokens().

tokenize_file(Filename) ->
    tokenize_file(Filename, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize_file/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize_file(file:filename_all(), map()) -> tokens().

tokenize_file(Filename, Opts) ->
    {ok, Bin} = file:read_file(Filename),
    tokenize(Bin, Opts).

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
        {true, {{MarkerId, _} = Marker, Expr, BinRest}} ->
            StateText = case string:trim(Text) of
                            <<>> -> State;
                            _ -> Eng:handle_text(OldPos, Text, State)
                        end,
            StateExpr = Eng:handle_expr(CurPos, MarkerId, Expr, StateText),
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
        fun({_, {StartMarker, _}} = Marker) ->
            StartMarkerBin = <<(list_to_binary(StartMarker))/binary, 32>>,
            StartMarkerLength = size(StartMarkerBin),
            case size(Bin) >= StartMarkerLength andalso
                 binary:split(Bin,
                              StartMarkerBin,
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
        fun({{_, {_, EndMarker}} = Marker, Bin}) ->
            EndMarkerBin = <<32, (list_to_binary(EndMarker))/binary>>,
            case size(Bin) >= size(EndMarkerBin) andalso
                 binary:split(Bin, EndMarkerBin)
            of
                [Expr, Rest] -> {true, {Marker, Expr, Rest}};
                _ -> false
            end
        end,
        Markers
    ).

best_match([Best | Rest], undefined) ->
    best_match(Rest, Best);
best_match([{_, Expr, _} = Best | Rest], {_, BestExpr, _})
    when size(Expr) < size(BestExpr)
->
    best_match(Rest, Best);
best_match([_ | Rest], Best) ->
    best_match(Rest, Best);
best_match([], Best) ->
    Best.

do_text_acc(<<"\n", T/binary>>, {Ln, _}, Text) ->
    {T, {Ln + 1, 1}, Text};
do_text_acc(<<H, T/binary>>, {Ln, Col}, Text) ->
    {T, {Ln, Col + 1}, <<Text/binary, H>>};
do_text_acc(<<>>, Pos, Text) ->
    {<<>>, Pos, Text}.

expr_position(Expr, {_, {StartMarker, EndMarker}}, OldPos) ->
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

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% TODO: Test position (check if trims are causing issues).
tokenize_test() ->
    Bin = <<"Hello,\n{{ World }}!">>,
    Expected = [
        {text, {{1, 1}, <<"Hello,">>}},
        {expr, {{2, 1}, var, <<"World">>}},
        {text, {{2, 11}, <<"!">>}}
    ],
    ?assertEqual(Expected, tokenize(Bin, #{engine => ?MODULE})).

tokenize_file_test() ->
    Filename = "/tmp/foo.eel",
    Bin = <<"\"Foo\"">>,
    ok = file:write_file(Filename, Bin),
    Expected = [{text, {{1, 2}, <<"\"Foo\"">>}}],
    ?assertEqual(Expected, tokenize_file(Filename, #{engine => ?MODULE})).

% Engine

markers() ->
    [{var, {"{{", "}}"}}].

init(#{}) ->
    [].

handle_expr({Ln, Col}, var, Expr, Acc) ->
    [{expr, {{Ln, Col}, var, Expr}} | Acc].

handle_text({Ln, Col}, Text, Acc) ->
    [{text, {{Ln, Col}, Text}} | Acc].

handle_body(Tokens) ->
    lists:reverse(Tokens).

-endif.
