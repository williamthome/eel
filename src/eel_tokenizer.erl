-module(eel_tokenizer).

%% API functions
-export([
    tokenize/1,
    tokenize/3
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([init/1, markers/0, handle_expr/4, handle_text/3, handle_body/1]).
-endif.

%% Defines
-define(DEFAULT_ENGINE, eel_smart_engine).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec tokenize(binary()) -> list().

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE, []).

-spec tokenize(binary(), module(), term()) -> list().

tokenize(Bin, Eng, Opts) ->
    State = Eng:init(Opts),
    do_tokenize(Bin, {1, 1}, <<>>, Eng, State).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% TODO: Sum line/column (position)
do_tokenize(<<>>, Pos, Text, Eng, State) ->
    StateEOF =
        case Text =:= <<>> of
            true -> State;
            false -> Eng:handle_text(Pos, Text, State)
        end,
    Eng:handle_body(StateEOF);
do_tokenize(Bin, Pos, Text, Eng, State) ->
    case retrieve_marker(Eng:markers(), Bin) of
        {true, {{StartMarker, EndMarker}, Expr, BinRest}} ->
            StateText =
                case Text =:= <<>> of
                    true -> State;
                    false -> Eng:handle_text(Pos, Text, State)
                end,
            StateExpr = Eng:handle_expr(Pos, {StartMarker, EndMarker}, Expr, StateText),
            do_tokenize(BinRest, Pos, <<>>, Eng, StateExpr);
        false ->
            {BinRest, TextAcc} =
                case Bin of
                    <<"\n", T/binary>> -> {T, Text};
                    <<H, T/binary>> -> {T, <<Text/binary, H>>};
                    <<>> -> {<<>>, Text}
                end,
            do_tokenize(BinRest, Pos, TextAcc, Eng, State)
    end.

retrieve_marker(EngMarkers, Bin) ->
    case match_markers_start(EngMarkers, Bin) of
        [] -> false;
        Markers ->
            case best_match(match_markers_end(Markers), undefined) of
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
                 binary:split(Bin, list_to_binary(StartMarker), [{scope, {0, StartMarkerLength}}])
            of
                [<<>>, Rest] -> {true, {Marker, Rest}};
                _ -> false
            end
        end,
        Markers
    ).

match_markers_end(Markers) ->
    lists:filtermap(
        fun({{StartMarker, EndMarker}, Bin}) ->
            case size(Bin) >= length(EndMarker) andalso
                 binary:split(Bin, list_to_binary(EndMarker))
            of
                [Expr, Rest] ->
                    {true, {{StartMarker, EndMarker}, string:trim(Expr), Rest}};
                _ -> false
            end
        end,
        Markers
    ).

best_match([Best | Markers], undefined) ->
    best_match(Markers, Best);
best_match([{_, Expr, _} = Best | Markers], {_, BestExpr, _})
  when size(Expr) < size(BestExpr) ->
    best_match(Markers, Best);
best_match([_ | Markers], Best) ->
    best_match(Markers, Best);
best_match([], Best) ->
    Best.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<"Hello,\n{{ World }}!">>,
    Expected = [
        {text, {{1, 1}, <<"Hello,">>}},
        {expr, {{1, 1}, {"{{", "}}"}, <<"World">>}},
        {text, {{1, 1}, <<"!">>}}
    ],
    ?assertEqual(Expected, tokenize(Bin, ?MODULE, [])).

% Engine

init([]) -> [].

markers() -> [{"{{", "}}"}].

handle_expr({Ln, Col}, {SMkr, EMkr}, Expr, Acc) ->
    [{expr, {{Ln, Col}, {SMkr, EMkr}, Expr}} | Acc].

handle_text({Ln, Col}, Text, Acc) ->
    [{text, {{Ln, Col}, Text}} | Acc].

handle_body(Tokens) ->
    lists:reverse(Tokens).

-endif.
