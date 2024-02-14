-module(eel_tokenizer).

-export([tokenize/1, tokenize/2, default_engines/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

-record( state,
       { engines  :: [engine()]
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
    Engines0 = maps:get(engines, Opts, default_engines()),
    Engines = lists:map(fun(Engine) ->
        {ok, EngineState} = Engine:init(Opts),
        EngineState#{module => Engine}
    end, Engines0),
    State = #state{
        engines = Engines,
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
                                    Buffer = State#state.buffer,
                                    do_tokenize(Rest, State#state{
                                        buffer = <<Buffer/binary, Expr/binary>>,
                                        tokens = [ExprTokens, TextTokens
                                                 | State#state.tokens ],
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
    StateTokens0 = lists:flatten(lists:reverse(State#state.tokens)),
    StateTokens = lists:filter(fun
        (#text_token{text = Text}) -> not is_string_empty(Text);
        (#expr_token{expr = Expr}) -> not is_string_empty(Expr)
    end, StateTokens0),
    lists:foldl(fun(#{module := Engine}, Tokens) ->
        Engine:handle_tokens(Tokens)
    end, StateTokens, State#state.engines);
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

is_string_empty(String) ->
    case string:trim(String) of
        [] -> true;
        <<>> -> true;
        _ -> false
    end.

handle_expr_start([#{markers := Markers} = Engine | Engines], Bin) ->
    case start_marker_match(Markers, Bin, []) of
        [] ->
            handle_expr_start(Engines, Bin);
        MatchMarkers ->
            {ok, {Engine, MatchMarkers}}
    end;
handle_expr_start([], _) ->
    none.

start_marker_match([#marker{start = Start} = Marker | Markers], Bin, Acc) ->
    case marker_match(Bin, Start) of
        {match, Text, Groups} ->
            start_marker_match(Markers, Bin, [{Marker#marker{start_groups = Groups}, Text} | Acc]);
        nomatch ->
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
    case marker_match(Bin, Final) of
        {match, Expr, Groups} ->
            {ok, {Marker#marker{final_groups = Groups}, Text, Expr}};
        nomatch ->
            end_marker_match(Markers, Bin)
    end;
end_marker_match([], _) ->
    none.

% TODO: Compile markers using re:compile to improve performance.
marker_match(Bin, Marker) ->
    case re:run(Bin, <<Marker/binary, "$">>, [{capture, all, binary}]) of
        {match, [Match0 | Groups]} ->
            Match = binary:part(Bin, 0, byte_size(Bin) - byte_size(Match0)),
            {match, Match, Groups};
        % {match, [{Length, _NChars} | _Groups]} ->
        %     {match, binary:part(Bin, 0, Length)};
        nomatch ->
            nomatch
    end.

handle_text([#{module := Engine} | Engines], Bin) ->
    case Engine:handle_text(Bin) of
        {ok, Tokens} ->
            {ok, resolve_handled_tokens(Tokens, [])};
        {error, Reason} ->
            {error, Reason};
        next ->
            handle_text(Engines, Bin)
    end;
handle_text([], Bin) ->
    {ok, [#text_token{text = Bin}]}.

handle_expr(#{module := Engine}, Marker, Bin) ->
    case Engine:handle_expr(Marker, Bin) of
        {ok, Tokens} ->
            {ok, resolve_handled_tokens(Tokens, [])};
        {error, Reason} ->
            {error, Reason}
    end.

resolve_handled_tokens([#text_token{} = TextToken | T], Acc0) ->
    Acc = [TextToken | Acc0],
    resolve_handled_tokens(T, Acc);
resolve_handled_tokens([#expr_token{} = ExprToken | T], Acc0) ->
    Acc = [ExprToken | Acc0],
    resolve_handled_tokens(T, Acc);
resolve_handled_tokens([], Acc) ->
    Acc.

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

tokenize_test() ->
    Expected = [{text_token,<<"<html><head><title>">>,undefined},
    {expr_token,<<"(maps:get(title, Assigns))">>,
                eel_smart_engine,
                {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
                        [push_token],
                        expr},
                [title],
                undefined},
    {text_token,<<"</title></head><body><ul>">>,undefined},
    {expr_token,<<"lists:map(fun(Item) ->">>,eel_smart_engine,
                {marker,expr_start,<<"<%=\\s+">>,<<"\\s+%>">>,
                        [],[],
                        [add_vertex,push_token,add_vertex],
                        expr_start},
                [],undefined},
    {text_token,<<"<li>">>,undefined},
    {expr_token,<<"(maps:get(item_prefix, Assigns))">>,
                eel_smart_engine,
                {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
                        [push_token],
                        expr},
                [item_prefix],
                undefined},
    {expr_token,<<"Item">>,eel_smart_engine,
                {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
                        [push_token],
                        expr},
                [],undefined},
    {text_token,<<"</li>">>,undefined},
    {expr_token,<<"end, (maps:get(items, Assigns)))">>,
                eel_smart_engine,
                {marker,expr_end,<<"<%\\s+">>,<<"\\s+.%>">>,[],
                        [],
                        [fetch_vertex_parent,push_token,
                         fetch_vertex_parent],
                        undefined},
                [items],
                undefined},
    {text_token,<<"</ul></body></html>">>,undefined}],

    Bin = <<
        "<html>"
        "<head>"
            "<title><%= @title .%></title>"
        "</head>"
        "<body>"
            "<ul>"
            "<%= lists:map(fun(Item) -> %>"
                "<li><%= @item_prefix .%><%= Item .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    Result = tokenize(Bin),
    ?assertEqual(Expected, Result).

-endif.
