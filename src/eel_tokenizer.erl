-module(eel_tokenizer).

-export([tokenize/1, tokenize/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

-record(state, { engines  :: [engine()]
               , buffer   :: binary()
               , text_acc :: binary()
               , tokens   :: [token() | [token()]]
               }).

-define(SMART_ENGINE, eel_smart_engine).

%%======================================================================
%% API functions
%%======================================================================

tokenize(Bin) ->
    tokenize(Bin, #{}).

tokenize(Input, Opts)
  when (is_binary(Input) orelse is_list(Input)), is_map(Opts) ->
    State = #state{
        engines = maps:get(engines, Opts, default_engines()),
        buffer = <<>>,
        text_acc = <<>>,
        tokens = []
    },
    do_tokenize(iolist_to_binary(Input), State).

%%======================================================================
%% Internal functions
%%======================================================================

default_engines() ->
    [?SMART_ENGINE].

do_tokenize(<<H, T/binary>>, State0) ->
    State = State0#state{
        buffer = <<(State0#state.buffer)/binary, H>>,
        text_acc = <<(State0#state.text_acc)/binary, H>>
    },
    case handle_expr_start(State#state.engines, State#state.text_acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Text, Expr, Rest}} ->
                    case handle_text(State#state.engines, Text) of
                        {ok, TextTokens} ->
                            case handle_expr(Engine, Marker, Expr) of
                                {ok, ExprTokens} ->
                                    do_tokenize(Rest, State#state{
                                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                                        tokens = [ExprTokens, TextTokens | State#state.tokens],
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
            do_tokenize(T, State)
    end;
do_tokenize(<<>>, #state{text_acc = <<>>} = State) ->
    lists:foldl(fun(Engine, Tokens) ->
        Engine:handle_tokens(lists:flatten(Tokens))
    end, lists:reverse(State#state.tokens), State#state.engines);
do_tokenize(<<>>, #state{text_acc = Text} = State) ->
    case handle_text(State#state.engines, Text) of
        {ok, Tokens} ->
            do_tokenize(<<>>, State#state{
                tokens = [Tokens | State#state.tokens],
                text_acc = <<>>
            });
        {error, Reason} ->
            {error, Reason}
    end.

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

handle_text(Engines, Bin) ->
    do_handle_text(Engines, Bin, []).

do_handle_text([Engine | Engines], Bin, Acc0) ->
    case Engine:handle_text(Bin) of
        {ok, Tokens} ->
            Acc = resolve_handled_tokens(Tokens, Engine, Acc0),
            do_handle_text(Engines, Bin, Acc);
        {error, Reason} ->
            {error, Reason}
    end;
do_handle_text([], _, Acc) ->
    {ok, Acc}.

handle_expr(Engine, Marker, Bin) ->
    case Engine:handle_expr(Marker, Bin) of
        {ok, Tokens} ->
            {ok, resolve_handled_tokens(Tokens, Engine, [])};
        {error, Reason} ->
            {error, Reason}
    end.

resolve_handled_tokens([{text, Text} | T], Engine, Acc0) ->
    Acc = [new_text_token(Text) | Acc0],
    resolve_handled_tokens(T, Engine, Acc);
resolve_handled_tokens([{expr, {Marker, Expr, Vars}} | T], Engine, Acc0) ->
    Acc = [new_expr_token(Expr, Engine, Marker, Vars) | Acc0],
    resolve_handled_tokens(T, Engine, Acc);
resolve_handled_tokens([], _, Acc) ->
    Acc.

new_text_token(Text) when is_binary(Text) ->
    #text_token{
        text = Text
    }.

new_expr_token(Expr, Engine, Marker, Vars) when is_binary(Expr)
                                              , is_list(Vars) ->
    #expr_token{
        expr = Expr,
        engine = Engine,
        marker = Marker,
        vars = Vars
    }.

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

tokenize_test() ->
    Expected = [{text_token,<<"<html><head><title>">>},
    {expr_token,<<"maps:get(title, Bindings)">>,eel_smart_engine,
                {marker,expr,<<"<%=">>,<<".%>">>,[push_token]},
                [title]},
    {text_token,<<"</title></head><body><ul>">>},
    {expr_token,<<"lists:map(fun(Item) ->">>,eel_smart_engine,
                {marker,expr_start,<<"<%=">>,<<"%>">>,
                        [add_vertex,push_token,add_vertex]},
                []},
    {expr_token,<<"TODO: Items to binary">>,eel_smart_engine,
                {marker,comment,<<"<%%">>,<<".%>">>,[ignore_token]},
                []},
    {text_token,<<"<li>">>},
    {expr_token,<<"maps:get(item_prefix, Bindings)">>,eel_smart_engine,
                {marker,expr,<<"<%=">>,<<".%>">>,[push_token]},
                [item_prefix]},
    {expr_token,<<"integer_to_binary(Item)">>,eel_smart_engine,
                {marker,expr,<<"<%=">>,<<".%>">>,[push_token]},
                []},
    {text_token,<<"</li>">>},
    {expr_token,<<"end, maps:get(items, Bindings))">>,eel_smart_engine,
                {marker,expr_end,<<"<%">>,<<".%>">>,
                        [fetch_vertex_parent,push_token,fetch_vertex_parent]},
                [items]},
    {text_token,<<"</ul></body></html>">>}],

    Bin = <<
        "<html>"
        "<head>"
            "<title><%= @title .%></title>"
        "</head>"
        "<body>"
            "<ul>"
            "<%= lists:map(fun(Item) -> %>"
                "<%% TODO: Items to binary .%>"
                "<li><%= @item_prefix .%><%= integer_to_binary(Item) .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    Result = tokenize(Bin),
    ?assertEqual(Expected, Result).

-endif.
