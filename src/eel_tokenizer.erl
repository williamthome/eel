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
         handle_expr/5, handle_text/4, handle_body/1]).
-endif.

%% Types
-export_type([tokens/0, result/0]).

%% Includes
-include("eel_core.hrl").
-include_lib("kernel/include/logger.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type tokens() :: {eel_engine:static(), eel_engine:dynamic()}.
-type result() :: {ok, tokens()} | {error, end_marker_not_found}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc tokenize/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary()) -> result().

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(binary(), map()) -> result().

tokenize(Bin, Opts) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    Index = 1,
    Pos = {1, 1},
    do_tokenize(Bin, Index, Pos, Pos, <<>>, Eng, State).

%% -----------------------------------------------------------------------------
%% @doc tokenize_file/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize_file(file:filename_all()) -> result().

tokenize_file(Filename) ->
    tokenize_file(Filename, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize_file/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize_file(file:filename_all(), map()) -> result().

tokenize_file(Filename, Opts) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            tokenize(Bin, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_tokenize(<<>>, Index, Pos, _, Text, Eng, State) ->
    try
        StateEOF = case Text =:= <<>> of
                       true -> State;
                       false -> Eng:handle_text(Index, Pos, Text, State)
                   end,
        Eng:handle_body(StateEOF)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace,
                text => Text,
                position => Pos,
                eof => true
            }),
            {error, Reason}
    end;
do_tokenize(Bin, Index, PrevPos, Pos, Text, Eng, State) ->
    try
        case retrieve_marker(Eng:markers(), Bin) of
            {true, {{MarkerId, _} = Marker, Expr, BinRest}} ->
                {NewIndex, StateText} =
                    case string:trim(Text) of
                        <<>> -> {Index, State};
                        _ -> {Index + 1, Eng:handle_text(Index, PrevPos, Text, State)}
                    end,
                TextPos = text_pos(Text, PrevPos),
                StateExpr = Eng:handle_expr(NewIndex, TextPos, MarkerId, Expr, StateText),
                NewPos = expr_position(Expr, Marker, TextPos),
                do_tokenize(BinRest, NewIndex + 1, NewPos, NewPos, <<>>, Eng, StateExpr);
            false ->
                {BinRest, NewPos, TextAcc} = do_text_acc(Bin, PrevPos, Text),
                do_tokenize(BinRest, Index, PrevPos, NewPos, TextAcc, Eng, State);
            {error, end_marker_not_found} ->
                error({end_marker_not_found, {Pos, Text}})
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace,
                text => Text,
                position => Pos,
                eof => false
            }),
            {error, Reason}
    end.

retrieve_marker(EngMarkers, Bin) ->
    case match_markers_start(EngMarkers, Bin) of
        [] -> false;
        Markers -> case best_match(match_markers_end(Markers), undefined) of
                       undefined -> {error, end_marker_not_found};
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

do_text_acc(<<$\n, T/binary>>, {Ln, _}, Text) ->
    {T, {Ln + 1, 1}, <<Text/binary, $\n>>};
do_text_acc(<<H, T/binary>>, {Ln, Col}, Text) ->
    {T, {Ln, Col + 1}, <<Text/binary, H>>};
do_text_acc(<<>>, Pos, Text) ->
    {<<>>, Pos, Text}.

expr_position(Expr, {_, {StartMarker, EndMarker}}, PrevPos) ->
    text_pos(iolist_to_binary([StartMarker, 32, Expr, 32, EndMarker]), PrevPos).

% FIXME: How to deal with position when text quoted?

text_pos(<<$\n, T/binary>>, {Ln, _}) ->
    text_pos(T, {Ln + 1, 1});
text_pos(<<_, T/binary>>, {Ln, Col}) ->
    text_pos(T, {Ln, Col + 1});
text_pos(<<>>, Pos) ->
    Pos.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<"{{ Hey }}!\nSay hello to {{ This }}\n{{ World }}!">>,
    Expected = [
        {expr,{{1,1},var,<<"Hey">>}},
        {text,{{1,10},<<"!\nSay hello to ">>}},
        {expr,{{2,14},var,<<"This">>}},
        {expr,{{3,1},var,<<"World">>}},
        {text,{{3,12},<<"!">>}}
    ],
    ?assertEqual(Expected, tokenize(Bin, #{engine => ?MODULE})).

tokenize_file_test() ->
    Filename = "/tmp/foo.eel",
    Bin = <<"\"Foo\"">>,
    ok = file:write_file(Filename, Bin),
    Expected = [{text, {{1, 1}, <<"\"Foo\"">>}}],
    ?assertEqual(Expected, tokenize_file(Filename, #{engine => ?MODULE})).

% Engine

markers() ->
    [{var, {"{{", "}}"}}].

init(#{}) ->
    [].

handle_expr(_Index, {Ln, Col}, var, Expr, Acc) ->
    [{expr, {{Ln, Col}, var, Expr}} | Acc].

handle_text(_Index, {Ln, Col}, Text, Acc) ->
    [{text, {{Ln, Col}, Text}} | Acc].

handle_body(Tokens) ->
    lists:reverse(Tokens).

-endif.
