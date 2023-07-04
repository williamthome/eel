%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl tokenizer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_tokenizer).

-compile(inline_list_funcs).
-compile({inline, [ do_tokenize/9
                  , find_end_marker/6
                  ]}).

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
-export([ init/1
        , handle_expr_start/2
        , handle_expr_end/3
        , handle_text/3
        , handle_body/2
        ]).
-endif.

%% Types
-type tokens() :: {eel_engine:static(), eel_engine:dynamic()}.
-type state()  :: eel_engine:state().
-type result() :: {ok, {tokens(), state()}} | {error, term()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc tokenize/1.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(iodata()) -> result().

tokenize(IoData) ->
    tokenize(IoData, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc tokenize/2.
%% @end
%% -----------------------------------------------------------------------------
-spec tokenize(iodata(), map()) -> result().

tokenize(Bin, Opts) when is_binary(Bin) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    case Eng:init(Opts) of
        {ok, InitState} ->
            case do_tokenize(Bin, Eng, InitState, 1, {1, 1}, {1, 1}, [], [], Opts) of
                {ok, {Tokens, EndState}} ->
                    Eng:handle_body(Tokens, EndState);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
tokenize(List, Opts) when is_list(List) ->
    tokenize(erlang:iolist_to_binary(List), Opts).

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

do_tokenize( <<H, T/binary>> = Bin
           , Eng
           , State
           , Index
           , PrevPos
           , {Ln, Col} = Pos
           , Text
           , Acc
           , Opts
           ) ->
    try
        case Eng:handle_expr_start(Bin, State) of
            {ok, {SMarker, SMarkerSize, StartExpr, NewState}} ->
                SExprPos = {Ln, Col + SMarkerSize},
                case find_end_marker(StartExpr, Eng, SMarker, SExprPos, [], NewState) of
                    {ok, {MarkerId, EndExprPos, Expr, Rest, ExprState}} ->
                        case Text of
                            [] ->
                                ExprToken = {Index, {MarkerId, Pos, Expr}},
                                do_tokenize( Rest
                                        , Eng
                                        , ExprState
                                        , Index + 1
                                        , EndExprPos
                                        , EndExprPos
                                        , []
                                        , [ExprToken | Acc]
                                        , Opts
                                        );
                            [$\n] ->
                                ExprToken = {Index, {MarkerId, {Ln, 1}, Expr}},
                                do_tokenize( Rest
                                        , Eng
                                        , ExprState
                                        , Index + 1
                                        , EndExprPos
                                        , EndExprPos
                                        , []
                                        , [ExprToken | Acc]
                                        , Opts
                                        );
                            Text ->
                                % NOTE: handle_text is using the ExprState, this can
                                %       be an issue because the text is placed before
                                %       the expression and not after.
                                case Eng:handle_text(lists:reverse(Text), PrevPos, ExprState) of
                                    {ok, {NewText, NewTextPos, TextState}} ->
                                        TextToken = {Index, {text, NewTextPos, NewText}},
                                        ExprToken = {Index + 1, {MarkerId, Pos, Expr}},
                                        do_tokenize( Rest
                                                , Eng
                                                , TextState
                                                , Index + 2
                                                , EndExprPos
                                                , EndExprPos
                                                , []
                                                , [ExprToken, TextToken | Acc]
                                                , Opts
                                                );
                                    {error, Reason} ->
                                        {error, Reason}
                                end
                        end;
                    {error, no_end_marker} ->
                        {error, {no_end_marker, {Pos, SMarker, Bin}}}
                end;
            {ok, NewState} ->
                NextPos = char_pos(H, Pos),
                do_tokenize( T
                        , Eng
                        , NewState
                        , Index
                        , PrevPos
                        , NextPos
                        , [H | Text]
                        , Acc
                        , Opts
                        );
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:ErrReason:Stacktrace ->
            ?LOG_ERROR(#{ class => Class
                        , reason => ErrReason
                        , stacktrace => Stacktrace
                        , module => ?MODULE
                        , function => tokenize
                        , arity => 2
                        , position => Pos
                        , rest => Bin
                        , eof => false
                        , options => Opts
                        }),
            erlang:raise(Class, ErrReason, Stacktrace)
    end;
do_tokenize(<<>>, _, State, _, _, _, [], Acc, _) ->
    {ok, {lists:reverse(Acc), State}};
do_tokenize(<<>>, Eng, State, Index, Pos, _, Text, Acc, Opts) ->
    try
        case Eng:handle_text(lists:reverse(Text), Pos, State) of
            {ok, {NewText, NewTextPost, TextState}} ->
                TextToken = {Index, {text, NewTextPost, NewText}},
                {ok, {lists:reverse([TextToken | Acc]), TextState}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:ErrReason:Stacktrace ->
            ?LOG_ERROR(#{ class => Class
                        , reason => ErrReason
                        , stacktrace => Stacktrace
                        , module => ?MODULE
                        , function => tokenize
                        , arity => 2
                        , position => Pos
                        , rest => <<>>
                        , eof => true
                        , options => Opts
                        }),
            erlang:raise(Class, ErrReason, Stacktrace)
    end.

find_end_marker( <<H, T/binary>> = Bin
               , Eng
               , SMarker
               , {Ln, Col} = PrevPos
               , Expr
               , State
               ) ->
    case Eng:handle_expr_end(SMarker, Bin, State) of
        {ok, {MarkerId, EndMarkerSize, Rest, NewState}} ->
            NewPrevPos = {Ln, Col + EndMarkerSize},
            {ok, {MarkerId, NewPrevPos, lists:reverse(Expr), Rest, NewState}};
        {ok, NewState} ->
            Acc = [H | Expr],
            NewPrevPos = char_pos(H, PrevPos),
            find_end_marker(T, Eng, SMarker, NewPrevPos, Acc, NewState);
        {error, Reason} ->
            {error, Reason}
    end;
find_end_marker(<<>>, _, _, _, _, _) ->
    {error, no_end_marker}.

char_pos($\n, {Ln, _}) ->
    {Ln + 1, 1};
char_pos(_, {Ln, Col}) ->
    {Ln, Col + 1}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin1 = <<"{{ Hey }}!\nSay hello to {{ This }}\n{{ World }}!">>,
    Expected1 = {ok, [
        {1,{var,{1,1}," Hey "}},
        {2,{text,{1,10},"!\nSay hello to "}},
        {3,{var,{2,14}," This "}},
        {4,{var,{3,1}," World "}},
        {5,{text,{3,12},"!"}}
    ]},
    Bin2 = <<"Foo">>,
    Expected2 = {ok, [
        {1,{text,{1,1},"Foo"}}
    ]},
    Bin3 = <<"{{ Foo }}{{ Bar }}Baz">>,
    Expected3 = {ok, [
        {1,{var,{1,1}," Foo "}},
        {2,{var,{1,10}," Bar "}},
        {3,{text,{1,19},"Baz"}}
    ]},
    Opts = #{engine => ?MODULE},
    [
        ?assertEqual(Expected1, tokenize(Bin1, Opts)),
        ?assertEqual(Expected2, tokenize(Bin2, Opts)),
        ?assertEqual(Expected3, tokenize(Bin3, Opts))
    ].

tokenize_file_test() ->
    Filename = "/tmp/foo.eel",
    Bin = <<"\"Foo\"">>,
    ok = file:write_file(Filename, Bin),
    Expected = [{1, {text,{1, 1},"\"Foo\""}}],
    Opts = #{engine => ?MODULE},
    {ok, Result} = tokenize_file(Filename, Opts),
    ?assertEqual(Expected, Result).

% Engine

init(#{}) ->
    {ok, []}.

handle_expr_start(<<"{{", Rest/binary>>, State) ->
    {ok, {<<"{{">>, 2, Rest, State}};
handle_expr_start(_, State) ->
    {ok, State}.

handle_expr_end(<<"{{">>, <<"}}", Rest/binary>>, State) ->
    {ok, {var, 2, Rest, State}};
handle_expr_end(_, _, State) ->
    {ok, State}.

handle_text(Text, Pos, State) ->
    {ok, {Text, Pos, State}}.

handle_body(Tokens, _State) ->
    {ok, Tokens}.

-endif.
