-module(eel_tokenizer).

-export([ tokenize/1
        , tokenize/2
        , default_engines/0
        , get_tokens/1
        , set_tokens/2
        , push_tokens/2
        , push_token/2
        , get_engine_state/2
        , set_engine_state/3
        , get_opts/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

-record( state,
       { engines  :: [engine()]
       , buffer   :: binary()
       , text_acc :: binary()
       , tokens   :: [token() | [token()]]
       , opts     :: opts()
       }).

% TODO: opts/0 fields.
-type opts() :: map().

-define(SMART_ENGINE, eel_smart_engine).

%%======================================================================
%% API functions
%%======================================================================

tokenize(Bin) ->
    tokenize(Bin, #{}).

tokenize(Input, Opts)
  when (is_binary(Input) orelse is_list(Input)), is_map(Opts) ->
    Engines0 = maps:get(engines, Opts, default_engines()),
    Engines = lists:foldl(fun(EngineDef, Acc) ->
        {Engine, EngineOpts} =
            case EngineDef of
                {EMod, EOpts} when is_atom(EMod) ->
                    {EMod, EOpts};
                EMod when is_atom(EMod) ->
                    {EMod, #{}}
            end,
        {ok, EngineState0} = Engine:init(EngineOpts),
        EngineState = normalize_engine_state(Engine, EngineState0),
        Acc#{Engine => EngineState}
    end, #{}, Engines0),
    State = #state{
        engines = Engines,
        buffer = <<>>,
        text_acc = <<>>,
        tokens = [],
        opts = Opts
    },
    do_tokenize(iolist_to_binary(Input), State).

default_engines() ->
    [?SMART_ENGINE].

get_tokens(#state{tokens = Tokens}) ->
    Tokens.

set_tokens(Tokens, State) ->
    State#state{tokens = Tokens}.

push_tokens(Tokens, State) ->
    lists:foldl(fun push_token/2, State, Tokens).

push_token(Token, State) ->
    State#state{tokens = [Token | State#state.tokens]}.

get_engine_state(Engine, State) ->
    maps:get(Engine, State#state.engines).

set_engine_state(Engine, EngineState, State) ->
    Engines = State#state.engines,
    case is_map_key(Engine, Engines) of
        true ->
            State#state{engines = Engines#{Engine => EngineState}};
        false ->
            error(badarg, [Engine, EngineState, State])
    end.

get_opts(#state{opts = Opts}) ->
    Opts.

%%======================================================================
%% Internal functions
%%======================================================================

normalize_engine_state(Module, EngineState) ->
    Markers = lists:map(fun(Marker) ->
        {ok, MarkerStart} = re:compile(<<(Marker#marker.start)/binary, "$">>),
        {ok, MarkerFinal} = re:compile(<<(Marker#marker.final)/binary, "$">>),
        Marker#marker{
            start = MarkerStart,
            final = MarkerFinal
        }
    end, EngineState#engine_state.markers),
    EngineState#engine_state{
        module = Module,
        markers = Markers
    }.

do_tokenize(<<H, T/binary>>, State0) ->
    State = State0#state{
        buffer = <<(State0#state.buffer)/binary, H>>,
        text_acc = <<(State0#state.text_acc)/binary, H>>
    },
    Engines = maps:values(State#state.engines),
    case handle_expr_start(Engines, State#state.text_acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Text, Expr, Rest}} ->
                    case handle_text(Engines, Text, State) of
                        {ok, TextState} ->
                            case handle_expr(Engine, Marker, Expr, TextState) of
                                {ok, ExprState} ->
                                    Buffer = ExprState#state.buffer,
                                    do_tokenize(Rest, ExprState#state{
                                        buffer = <<Buffer/binary, Expr/binary>>,
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
do_tokenize(<<>>, #state{text_acc = <<>>} = State0) ->
    Tokens0 = lists:flatten(lists:reverse(State0#state.tokens)),
    Tokens = lists:filter(fun
        (#text_token{text = Text}) -> not is_string_empty(Text);
        (#expr_token{expr = Expr}) -> not is_string_empty(Expr)
    end, Tokens0),
    State = State0#state{tokens = Tokens},
    Engines = maps:values(State#state.engines),
    handle_tokens(Engines, State);
do_tokenize(<<>>, #state{text_acc = Text} = State0) ->
    Engines = maps:values(State0#state.engines),
    case handle_text(Engines, Text, State0) of
        {ok, State} ->
            do_tokenize(<<>>, State#state{
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

handle_expr_start([#engine_state{markers = Markers} = Engine | Engines], Bin) ->
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

marker_match(Bin, RE) ->
    case re:run(Bin, RE, [{capture, all, binary}]) of
        {match, [Match0 | Groups]} ->
            Match = binary:part(Bin, 0, byte_size(Bin) - byte_size(Match0)),
            {match, Match, Groups};
        % {match, [{Length, _NChars} | _Groups]} ->
        %     {match, binary:part(Bin, 0, Length)};
        nomatch ->
            nomatch
    end.

handle_text([#engine_state{module = Engine} | Engines], Bin0, State0) ->
    case Engine:handle_text(Bin0, State0) of
        {ok, Bin, State} ->
            handle_text(Engines, Bin, State);
        {halt, State} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end;
handle_text([], Bin, State0) ->
    State = push_token(#text_token{text = Bin}, State0),
    {ok, State}.

handle_expr(#engine_state{module = Engine}, Marker, Bin, State0) ->
    case Engine:handle_expr(Marker, Bin, State0) of
        {ok, State} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

handle_tokens([#engine_state{module = Engine} | Engines], State0) ->
    case Engine:handle_tokens(State0) of
        {ok, State} ->
            handle_tokens(Engines, State);
        {error, Reason} ->
            {error, Reason}
    end;
handle_tokens([], State) ->
    {State#state.tokens, State}.

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

tokenize_test() ->
    Expected = [{text_token,<<"<html><head><title>">>,undefined},
    {expr_token,<<"(maps:get(title, Assigns))">>,
        eel_smart_engine,
        {marker,expr,
            {re_pattern,0,0,0,
                <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,
                  255,255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,12,29,60,29,37,29,61,87,
                  9,25,120,0,12,0>>},
            {re_pattern,0,0,0,
                <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,
                  255,255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,11,87,9,12,29,37,29,62,
                  25,120,0,11,0>>},
            [],[],
            [push_token],
            expr},
        [title],
        undefined},
    {text_token,<<"</title></head><body><ul>">>,undefined},
    {expr_token,<<"lists:map(fun(Item) ->">>,eel_smart_engine,
        {marker,expr_start,
            {re_pattern,0,0,0,
                <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,
                  255,255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,12,29,60,29,37,29,61,87,
                  9,25,120,0,12,0>>},
            {re_pattern,0,0,0,
                <<69,82,67,80,78,0,0,0,0,0,0,0,65,0,0,0,255,255,
                  255,255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,10,95,9,29,37,29,62,25,
                  120,0,10,0>>},
            [],[],
            [add_vertex,push_token,add_vertex],
            expr_start},
        [],undefined},
    {text_token,<<"<li>">>,undefined},
    {expr_token,<<"(maps:get(item_prefix, Assigns))">>,
        eel_smart_engine,
        {marker,expr,
            {re_pattern,0,0,0,
                <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,
                  255,255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,12,29,60,29,37,29,61,87,
                  9,25,120,0,12,0>>},
            {re_pattern,0,0,0,
                <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,
                  255,255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,11,87,9,12,29,37,29,62,
                  25,120,0,11,0>>},
            [],[],
            [push_token],
            expr},
        [item_prefix],
        undefined},
    {expr_token,<<"Item">>,eel_smart_engine,
        {marker,expr,
            {re_pattern,0,0,0,
                <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,
                  255,255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,12,29,60,29,37,29,61,87,
                  9,25,120,0,12,0>>},
            {re_pattern,0,0,0,
                <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,
                  255,255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,11,87,9,12,29,37,29,62,
                  25,120,0,11,0>>},
            [],[],
            [push_token],
            expr},
        [],undefined},
    {text_token,<<"</li>">>,undefined},
    {expr_token,<<"end, (maps:get(items, Assigns)))">>,
        eel_smart_engine,
        {marker,expr_end,
            {re_pattern,0,0,0,
                <<69,82,67,80,78,0,0,0,0,0,0,0,81,0,0,0,255,255,
                  255,255,255,255,255,255,60,0,37,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,10,29,60,29,37,87,9,25,
                  120,0,10,0>>},
            {re_pattern,0,0,0,
                <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,
                  255,255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,
                  64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,131,0,11,87,9,12,29,37,29,62,
                  25,120,0,11,0>>},
            [],[],
            [fetch_vertex_parent,push_token,fetch_vertex_parent],
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
    {Result, _State} = tokenize(Bin),
    ?assertEqual(Expected, Result).

-endif.
