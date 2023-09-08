-module(v2_eel_tokenizer).

-export([tokenize/1, tokenize/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("v2_eel.hrl").

-record(state, { engines   :: [engine()]
               , buffer    :: binary()
               , text_acc  :: binary()
               , tokens    :: [token()]
               , converter :: module()
               }).

-record(text_token, { text :: binary()
                    , handled_text :: binary()
                    }).

-record(expr_token, { expr :: binary()
                    , handled_expr :: binary()
                    , engine :: engine()
                    , marker :: #marker{} | none
                    , vars :: [atom()]
                    }).

-type engine()     :: module().
% -type text_token() :: {text, binary()}.
% -type expr_token() :: {expr, {engine(), marker_id(), binary()}}.
% -type token() :: text_token() | expr_token().

% -type token() :: {text | expr, binary()}.

-type text_token() :: #text_token{}.
-type expr_token() :: #expr_token{}.
-type token() :: text_token() | expr_token().

-define(SMART_ENGINE, v2_eel_smart_engine).
-define(CONVERTER, v2_eel_converter).

%%%=============================================================================
%%% API functions
%%%=============================================================================

tokenize(Bin) ->
    tokenize(Bin, #{}).

tokenize(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    State = #state{
        engines = maps:get(engines, Opts, default_engines()),
        buffer = <<>>,
        text_acc = <<>>,
        tokens = [],
        converter = atom_to_binary(maps:get(converter, Opts, default_converter()))
    },
    do_tokenize(Bin, State).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

default_engines() ->
    [?SMART_ENGINE].

default_converter() ->
    ?CONVERTER.

do_tokenize(<<H, T/binary>>, State0) ->
    State = State0#state{
        buffer = <<(State0#state.buffer)/binary, H>>,
        text_acc = <<(State0#state.text_acc)/binary, H>>
    },
    case handle_expr_start(State#state.engines, State#state.text_acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Text, Expr, Rest}} ->
                    case handle_text(State#state.engines, Text, State#state.converter) of
                        {ok, FirstToken} ->
                            case handle_expr(Engine, Marker, Expr, State#state.converter) of
                                {ok, SecondToken} ->
                                    do_tokenize(Rest, State#state{
                                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                                        tokens = [SecondToken, FirstToken | State#state.tokens],
                                        text_acc = <<>>
                                    });
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                none ->
                    % TODO: Check if should just skip when no end marker found.
                    %       e.g: do_tokenize(T, State)
                    error({noendmarker, #{
                        engine => Engine,
                        markers_candidate => Markers,
                        buffer => State#state.buffer,
                        text => State#state.text_acc
                    }})
            end;
        none ->
            do_tokenize(T, State);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<>>, #state{text_acc = <<>>} = State) ->
    lists:foldl(fun(Engine, Tokens) ->
                    Engine:handle_tokens(Tokens)
                end, lists:reverse(State#state.tokens), State#state.engines);
do_tokenize(<<>>, #state{text_acc = Text} = State) ->
    do_tokenize(<<>>, State#state{
        tokens = [{text, Text} | State#state.tokens],
        text_acc = <<>>
    }).

handle_expr_start([Engine | Engines], Bin) ->
    case start_marker_match(Engine:markers(), Bin, []) of
        [] ->
            handle_expr_start(Engines, Bin);
        Markers ->
            {ok, {Engine, Markers}}
    end;
handle_expr_start([], _) ->
    none.

start_marker_match([#marker{start = Start} = Marker | Markers], Bin, Acc) ->
    BSize = erlang:byte_size(Bin),
    MSize = erlang:byte_size(Start),
    case Bin of
        <<Start:MSize/binary, 32>> ->
            start_marker_match(Markers, Bin, [{Marker, <<>>} | Acc]);
        <<Text:(BSize-MSize-1)/binary, Start:MSize/binary, 32>> ->
            start_marker_match(Markers, Bin, [{Marker, Text} | Acc]);
        _ ->
            start_marker_match(Markers, Bin, Acc)
    end;
start_marker_match([], _, Acc) ->
    Acc.

handle_expr_end(<<H, T/binary>>, Markers, Acc0) ->
    Acc = <<Acc0/binary, H>>,
    case end_marker_match(Markers, Acc) of
        {ok, {Marker, Text, Expr}} ->
            {ok, {Marker, Text, Expr, T}};
        none ->
            handle_expr_end(T, Markers, Acc)
    end;
handle_expr_end(<<>>, _, _) ->
    none.

end_marker_match([{#marker{final = Final} = Marker, Text} | Markers], Bin) ->
    BSize = erlang:byte_size(Bin),
    MSize = erlang:byte_size(Final),
    case Bin of
        <<Expr:(BSize-MSize-1)/binary, 32, Final:MSize/binary>> ->
            {ok, {Marker, Text, Expr}};
        _ ->
            end_marker_match(Markers, Bin)
    end;
end_marker_match([], _) ->
    none.

handle_text(Engines, Bin, Converter) ->
    handle_text(Engines, Bin, Bin, Converter).

handle_text([Engine | Engines], Bin, RawBin, Converter) ->
    case Engine:handle_text(Bin, Converter) of
        {ok, {text, Text}} when is_binary(Text) ->
            handle_text(Engines, Text, RawBin, Converter);
        {ok, {expr, {Expr, Vars}}} when is_binary(Expr), is_list(Vars) ->
            {ok, #expr_token{
                expr = RawBin,
                handled_expr = Expr,
                engine = Engine,
                marker = none,
                vars = Vars
            }};
        {error, Reason} ->
            {error, Reason}
    end;
handle_text([], Text, RawBin, _) ->
    {ok, #text_token{
        text = RawBin,
        handled_text = Text
    }}.

handle_expr(Engine, Marker, Bin, Converter) ->
    case Engine:handle_expr(Marker, Bin, Converter) of
        {ok, {text, Text}} when is_binary(Text) ->
            {ok, #text_token{
                text = Bin,
                handled_text = Text
            }};
        {ok, {expr, {Expr, Vars}}} when is_binary(Expr), is_list(Vars) ->
            {ok, #expr_token{
                expr = Bin,
                handled_expr = Expr,
                engine = Engine,
                marker = Marker,
                vars = Vars
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Expected = [
        {text,<<"Hello, ">>},
        {expr, {?SMART_ENGINE,expr,<<"World">>}},
        {text,<<"!<p>">>},
        {expr, {?SMART_ENGINE,expr_start,<<"case Bool of">>}},
        {text,<<>>},
        {expr, {?SMART_ENGINE,expr_continue,<<"true ->">>}},
        {text,<<"True">>},
        {expr, {?SMART_ENGINE,expr_continue,<<"; false ->">>}},
        {text,<<>>},
        {expr, {?SMART_ENGINE,expr_start,<<"case Foo of ->">>}},
        {text,<<>>},
        {expr, {?SMART_ENGINE,expr_continue,<<"foo ->">>}},
        {text,<<"Foo">>},
        {expr, {?SMART_ENGINE,expr_continue,<<"Bar ->">>}},
        {text,<<"Bar">>},
        {expr, {?SMART_ENGINE,expr_end,<<"end">>}},
        {text,<<>>},
        {expr, {?SMART_ENGINE,expr_end,<<"end">>}},
        {text,<<"</p>">>}
    ],
    Bin = <<
        "Hello, <%= @world .%>!"
        "<p>"
            "<%= case @bool of %>"
            "<% true -> %>"
                "True"
            "<% ; false -> %>"
                "<%= case @foo of -> %>"
                "<% foo -> %>"
                    "Foo"
                "<% ; _ -> %>"
                    "<p><%= @bar .%></p>"
                "<% end .%>"
            "<% end .%>"
        "</p>"
    >>,
    ?assertEqual(Expected, tokenize(Bin)).

-endif.
