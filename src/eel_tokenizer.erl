%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl tokenizer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_tokenizer).

%% API functions
-export([ tokenize/1
        , tokenize/2
        , tokenize_file/1
        , tokenize_file/2
        ]).

%% Types
-export_type([ tokens/0
             , result/0
             ]).

%% Includes
-include("eel_core.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ markers/0, init/1, handle_expr/5, handle_text/4, handle_body/1 ]).
-endif.

%% Types
-type tokens() :: {eel_engine:static(), eel_engine:dynamic()}.
-type result() :: {ok, tokens()} | {error, no_end_marker}.

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
    case Eng:init(Opts) of
        {ok, State} ->
            Index = 1,
            Pos = {1, 1},
            do_tokenize(Bin, Index, Pos, Pos, <<>>, Eng, State);
        {error, Reason} ->
            {error, Reason}
    end.

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

% TODO: Split into functions to be more readable

do_tokenize(<<>>, Index, Pos, _, Text, Eng, State) ->
    try
        HandleText =
            case Text =:= <<>> of
                true -> {ok, State};
                false -> Eng:handle_text(Index, Pos, Text, State)
            end,
        case HandleText of
            {ok, StateEOF} ->
                Eng:handle_body(StateEOF);
            {error, Reason0} ->
                {error, Reason0}
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{ class => Class
                        , reason => Reason
                        , stacktrace => Stacktrace
                        , text => Text
                        , position => Pos
                        , engine => Eng
                        , eof => true
                        }),
            {error, Reason}
    end;
do_tokenize(Bin, Index, PrevPos, Pos, Text, Eng, State) ->
    try
        case retrieve_marker(Eng:markers(), Bin) of
            {true, {{MarkerId, _} = Marker, Expr, BinRest}} ->
                HandleText =
                    case string:trim(Text) of
                        <<>> ->
                            {ok, {Index, State}};
                        _ ->
                            case Eng:handle_text(Index, PrevPos, Text, State) of
                                {ok, HandleTextState} ->
                                    {ok, {Index + 1, HandleTextState}};
                                {error, Reason0} ->
                                    {error, Reason0}
                            end
                    end,
                case HandleText of
                    {ok, {NewIndex, NewState}} ->
                        TextPos = text_pos(Text, PrevPos),
                        case Eng:handle_expr(NewIndex, TextPos, MarkerId, Expr, NewState) of
                            {ok, StateExpr} ->
                                NewPos = expr_position(Expr, Marker, TextPos),
                                do_tokenize(BinRest, NewIndex + 1, NewPos, NewPos, <<>>, Eng, StateExpr);
                            {error, Reason1} ->
                                {error, Reason1}
                        end;
                    {error, Reason2} ->
                        {error, Reason2}
                end;
            false ->
                {BinRest, NewPos, TextAcc} = do_text_acc(Bin, PrevPos, Text),
                do_tokenize(BinRest, Index, PrevPos, NewPos, TextAcc, Eng, State);
            {error, no_end_marker} ->
                {error, {no_end_marker, {Pos, Text}}}
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{ class => Class
                        , reason => Reason
                        , stacktrace => Stacktrace
                        , text => Text
                        , position => Pos
                        , engine => Eng
                        , eof => false
                        }),
            {error, Reason}
    end.

retrieve_marker(EngMarkers, Bin) ->
    case match_markers_start(EngMarkers, Bin) of
        [] ->
            false;
        Markers ->
            EndMarkers = match_markers_end(Markers),
            case best_match(EndMarkers, undefined) of
                undefined ->
                    {error, no_end_marker};
                Marker ->
                    {true, Marker}
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
                 binary:split( Bin
                             , StartMarkerBin
                             , [ {scope, {0, StartMarkerLength}} ] )
            of
                [<<>>, Rest] ->
                    {true, {Marker, Rest}};
                _ ->
                     false
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
    when size(Expr) < size(BestExpr) ->
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
    {ok, Result} = tokenize(Bin, #{engine => ?MODULE}),
    ?assertEqual(Expected, Result).

tokenize_file_test() ->
    Filename = "/tmp/foo.eel",
    Bin = <<"\"Foo\"">>,
    ok = file:write_file(Filename, Bin),
    Expected = [{text, {{1, 1}, <<"\"Foo\"">>}}],
    {ok, Result} = tokenize_file(Filename, #{engine => ?MODULE}),
    ?assertEqual(Expected, Result).

% Engine

markers() ->
    [{var, {"{{", "}}"}}].

init(#{}) ->
    {ok, []}.

handle_expr(_Index, {Ln, Col}, var, Expr, Acc) ->
    {ok, [{expr, {{Ln, Col}, var, Expr}} | Acc]}.

handle_text(_Index, {Ln, Col}, Text, Acc) ->
    {ok, [{text, {{Ln, Col}, Text}} | Acc]}.

handle_body(Tokens) ->
    {ok, lists:reverse(Tokens)}.

-endif.
