-module(v2_eel_tokenizer).

-export([tokenize/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("v2_eel.hrl").

-record(state, {
    engines :: [atom()],
    buffer :: binary(),
    acc :: binary(),
    static :: [binary()],
    dynamic :: [binary()]
}).

tokenize(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    Engines = maps:get(engines, Opts, default_engines()),
    State = #state{
        engines = Engines,
        buffer = <<>>,
        acc = <<>>,
        static = [],
        dynamic = []
    },
    do_tokenize(Bin, State).

default_engines() ->
    [v2_eel_smart_engine].

do_tokenize(<<H, T/binary>>, #state{buffer = Buffer, acc = Acc} = State0) ->
    State = State0#state{
        buffer = <<Buffer/binary, H>>,
        acc = <<Acc/binary, H>>
    },
    case handle_expr_start(State#state.engines, State#state.acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Static, Dynamic, Rest}} ->
                    case Marker#marker.kind of
                        singleton ->
                            % We are fine here
                            ok;
                        multiplex ->
                            % TODO: Handle nested
                            ok;
                        snapshot ->
                            % TODO: Handle snapshot
                            ok
                    end,
                    do_tokenize(Rest, State#state{
                        buffer = <<(State#state.buffer)/binary, Static/binary, Dynamic/binary>>,
                        acc = <<>>,
                        static = [Static | State#state.static],
                        dynamic = [{Engine, Marker, Dynamic} | State#state.dynamic]
                    });
                none ->
                    % TODO: Should raise?
                    do_tokenize(T, State)
            end;
        none ->
            do_tokenize(T, State);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<>>, State) ->
    State#state{acc = <<>>}.

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

end_marker_match([{#marker{'end' = End} = Marker, Text} | Markers], Bin) ->
    BSize = erlang:byte_size(Bin),
    MSize = erlang:byte_size(End),
    case Bin of
        <<Expr:(BSize-MSize-1)/binary, 32, End:MSize/binary>> ->
            {ok, {Marker, Text, Expr}};
        _ ->
            end_marker_match(Markers, Bin)
    end;
end_marker_match([], _) ->
    none.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<"Hello, <%= World .%>!">>,
    Opts = #{},
    ?assertEqual(error, tokenize(Bin, Opts)).

-endif.
