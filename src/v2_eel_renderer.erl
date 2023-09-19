-module(v2_eel_renderer).

-export([render_all/2, render_changes/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API functions
%%%=============================================================================

render_all(Bindings, State) ->
    Indexes = maps:get(dynamics, State),
    Parts = maps:get(parts, State),
    eval_parts(Indexes, Parts, #{'Bindings' => Bindings}).

render_changes(Bindings, State) ->
    Indexes = get_vars_indexes(maps:get(vars, State), Bindings),
    Parts = maps:get(parts, State),
    eval_parts(Indexes, Parts, #{'Bindings' => Bindings}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_vars_indexes(Vars, Bindings) ->
    sets:to_list(
        lists:foldl(fun(Var, Set0) ->
            Indexes = proplists:get_all_values(Var, Vars),
            lists:foldl(fun(Index, Set) ->
                sets:add_element(Index, Set)
            end, Set0, Indexes)
        end, sets:new([{version, 2}]), maps:keys(Bindings))
    ).

eval_parts(Indexes, Parts, Bindings) ->
    lists:foldl(fun(Index, Acc) ->
        Acc#{Index => eval_part(Index, Parts, Bindings)}
    end, #{}, Indexes).

eval_part(Index, Parts, Bindings) ->
    Expr = maps:get(Index, Parts),
    case erl_eval:exprs(Expr, Bindings) of
        {value, IOData, _} when is_binary(IOData) ->
            IOData;
        {value, [IOData], _} ->
            IOData;
        {value, IOData, _} ->
            % NOTE: Here we have a list that can be optimized to the changes return.
            %       We need to know the static x dynamics of the expression.
            % ?debugFmt("[TODO: Optimize list] ~p~n", [{Index, IOData}]),
            IOData
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

render_test() ->
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
    Tokens = v2_eel_tokenizer:tokenize(Bin),
    Tree = v2_eel_structurer:tree(Tokens),

    ExpectedAll = #{1 => <<"EEl">>,
    3 =>
        [[<<"<li>">>,<<"Item - ">>,<<"1">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"2">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"3">>,<<"</li>">>]]},
    BindingsAll = #{title => <<"EEl">>, items => [1,2,3], item_prefix => <<"Item - ">>},
    StateAll = v2_eel_compiler:compile(Tree),
    ResultAll = render_all(BindingsAll, StateAll),
    ?assertEqual(ExpectedAll, ResultAll),

    ExpectedChanges = #{1 => <<"EEl - Embedded Erlang">>},
    BindingsChanges = #{title => <<"EEl - Embedded Erlang">>},
    StateChanges = maps:merge(StateAll, ResultAll),
    ResultChanges = render_changes(BindingsChanges, StateChanges),
    ?assertEqual(ExpectedChanges, ResultChanges).

-endif.
