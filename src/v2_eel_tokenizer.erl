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
    tree :: term(),
    tree_curr_vertex :: term()
}).

tokenize(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    Engines = maps:get(engines, Opts, default_engines()),
    Tree = digraph:new([acyclic]),
    State = #state{
        engines = Engines,
        buffer = <<>>,
        acc = <<>>,
        tree = Tree,
        tree_curr_vertex = digraph:add_vertex(Tree)
    },
    do_tokenize(Bin, State).

    % a()->
    %     [
    %         {expr, singleton},
    %         %{expr_start, multiplex}
    %         [
    %             {expr_start, open},
    %             {text, <<>>},
    %             {expr_continue, continue},
    %             {expr, continue},
    %             %{expr_start, multiplex}
    %             [
    %                 {expr_start, ok},
    %                 {text, <<>>},
    %                 {expr_end, ok}
    %             ],
    %             {expr_end, close}
    %         ],
    %         {text, <<>>} % continue
    %     ].

default_engines() ->
    [v2_eel_smart_engine].

do_tokenize(<<H, T/binary>>, #state{buffer = Buffer, acc = Acc} = State0) ->
    State1 = State0#state{
        buffer = <<Buffer/binary, H>>,
        acc = <<Acc/binary, H>>
    },
    case handle_expr_start(State1#state.engines, State1#state.acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Text, Expr, Rest}} ->
                    TreeTextVertex = digraph:add_vertex(State1#state.tree, {text, Text}),
                    digraph:add_edge(State1#state.tree, State1#state.tree_curr_vertex, TreeTextVertex),
                    % State2 = State1#state{
                    %     tree_acc = [{text, Text} | State1#state.tree_acc]
                    % },
                    State =
                        case Marker#marker.tree_behavior of
                            open ->
                                TreeCurrVertex = digraph:add_vertex(State1#state.tree),
                                digraph:add_edge(State1#state.tree, State1#state.tree_curr_vertex, TreeCurrVertex),
                                TreeExprVertex = digraph:add_vertex(State1#state.tree, {Engine, Marker#marker.id, Expr}),
                                digraph:add_edge(State1#state.tree, TreeCurrVertex, TreeExprVertex),
                                State1#state{
                                    tree_curr_vertex = TreeCurrVertex
                                };
                                % State2#state{
                                %     tree = [State2#state.tree_acc | State2#state.tree],
                                %     tree_acc = [{Marker, Expr}]
                                % };
                            push ->
                                TreeExprVertex = digraph:add_vertex(State1#state.tree, {Engine, Marker#marker.id, Expr}),
                                digraph:add_edge(State1#state.tree, State1#state.tree_curr_vertex, TreeExprVertex),
                                State1;
                                % State2#state{
                                %     tree_acc = [{Marker, Expr} | State2#state.tree_acc]
                                % };
                            close ->
                                TreeExprVertex = digraph:add_vertex(State1#state.tree, {Engine, Marker#marker.id, Expr}),
                                digraph:add_edge(State1#state.tree, State1#state.tree_curr_vertex, TreeExprVertex),
                                State1#state{
                                    tree_curr_vertex = hd(digraph:in_neighbours(State1#state.tree, State1#state.tree_curr_vertex))
                                };
                                % State2#state{
                                %     tree = [[{Marker, Expr} | State2#state.tree_acc] | State2#state.tree],
                                %     tree_acc = []
                                % };
                            ignore ->
                                State1
                        end,
                    do_tokenize(Rest, State#state{
                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                        acc = <<>>
                    });
                none ->
                    % TODO: Should raise?
                    do_tokenize(T, State1)
            end;
        none ->
            do_tokenize(T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<>>, #state{acc = <<>>} = State) ->

    {yes, Root} = digraph_utils:arborescence_root(State#state.tree),
    ?debugFmt("~p~n", [{
        digraph_utils:topsort(State#state.tree),
        digraph:out_neighbours(State#state.tree, Root)
    }]),


    State;
do_tokenize(<<>>, #state{acc = Text} = State) ->
    TreeTextVertex = digraph:add_vertex(State#state.tree, {text, Text}),
    digraph:add_edge(State#state.tree, State#state.tree_curr_vertex, TreeTextVertex),
    do_tokenize(<<>>, State#state{acc = <<>>}).

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
    Bin = <<
        "Hello, <%= World .%>!"
        "<p><%= case Bool of %>"
        "<% true -> %>True"
        "<% ; false -> %>False"
        "<% end .%>"
    >>,
    Opts = #{},
    ?assertEqual(error, tokenize(Bin, Opts)).

-endif.
