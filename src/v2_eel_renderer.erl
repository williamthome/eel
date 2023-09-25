-module(v2_eel_renderer).

-export([ render/2
        , render/3
        , render_changes/2
        , render_changes/3
        , get_vars_indexes/2
        , get_vars_from_indexes/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API functions
%%%=============================================================================

render(Bindings, State) ->
    render(Bindings, State, fun v2_eel_converter:to_string/1).

render(Bindings, State, ToStringFun) ->
    Indexes = maps:get(dynamics, State),
    eval_parts(Indexes, Bindings, State, ToStringFun).

render_changes(Bindings, State) ->
    render_changes(Bindings, State, fun v2_eel_converter:to_string/1).

render_changes(Bindings, State, ToStringFun) ->
    Indexes = get_vars_indexes(Bindings, State),
    eval_parts(Indexes, Bindings, State, ToStringFun).

get_vars_indexes(Bindings, State) ->
    do_get_vars_indexes(Bindings, maps:get(vars, State)).

get_vars_from_indexes(Indexes, State) ->
    lists:filtermap(fun({Var, Index}) ->
        case lists:member(Index, Indexes) of
            true ->
                {true, {Index, Var}};
            false ->
                false
        end
    end, maps:get(vars, State)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_get_vars_indexes(Bindings, Vars) ->
    sets:to_list(
        lists:foldl(fun(Var, Set0) ->
            Indexes = proplists:get_all_values(Var, Vars),
            lists:foldl(fun(Index, Set) ->
                sets:add_element(Index, Set)
            end, Set0, Indexes)
        end, sets:new([{version, 2}]), maps:keys(Bindings))
    ).

eval_parts(Indexes, Bindings, State, ToStringFun) ->
    Parts = maps:get(parts, State),
    do_eval_parts(Indexes, Parts, #{'Bindings' => Bindings}, ToStringFun).

do_eval_parts(Indexes, Parts, Bindings, ToStringFun) ->
    lists:foldl(fun(Index, Acc) ->
        Acc#{Index => eval_part(Index, Parts, Bindings, ToStringFun)}
    end, #{}, Indexes).

eval_part(Index, Parts, Bindings, ToStringFun) ->
    Expr = maps:get(Index, Parts),
    case erl_eval:exprs(Expr, Bindings) of
        {value, IOData, _} when is_binary(IOData) ->
            IOData;
        {value, [IOData], _} when is_binary(IOData) ->
            IOData;
        {value, [Term], _} ->
            ToStringFun(Term);
        {value, IOData, _} when is_list(IOData) ->
            % NOTE: Here we have a list that can be optimized to the changes return.
            %       We need to know the static x dynamics of the expression.
            % ?debugFmt("[TODO: Optimize list] ~p~n", [{Index, IOData}]),
            IOData;
        {value, Term, _} ->
            ToStringFun(Term)
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
    ResultAll = render(BindingsAll, StateAll),
    ?assertEqual(ExpectedAll, ResultAll),

    ExpectedChanges = #{1 => <<"EEl - Embedded Erlang">>},
    BindingsChanges = #{title => <<"EEl - Embedded Erlang">>},
    StateChanges = maps:merge(StateAll, ResultAll),
    ResultChanges = render_changes(BindingsChanges, StateChanges),
    ?assertEqual(ExpectedChanges, ResultChanges).

-endif.
