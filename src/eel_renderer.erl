-module(eel_renderer).

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

%%======================================================================
%% API functions
%%======================================================================

render(Assigns, State) ->
    render(Assigns, #{}, State).

render(Assigns, State, Opts) ->
    IsPartialRender = false,
    Indexes = maps:get(dynamics, State),
    eval_parts(IsPartialRender, Indexes, Assigns, State, Opts).

render_changes(Assigns, State) ->
    render_changes(Assigns, State, #{}).

render_changes(Assigns, State, Opts) ->
    IsPartialRender = true,
    Indexes = get_vars_indexes(Assigns, State),
    eval_parts(IsPartialRender, Indexes, Assigns, State, Opts).

get_vars_indexes(Assigns, State) ->
    Vars = maps:get(vars, State),
    sets:to_list(
        lists:foldl(fun(Var, Set0) ->
            Indexes = proplists:get_all_values(Var, Vars),
            lists:foldl(fun(Index, Set) ->
                sets:add_element(Index, Set)
            end, Set0, Indexes)
        end, sets:new([{version, 2}]), maps:keys(Assigns))
    ).

get_vars_from_indexes(Indexes, State) ->
    lists:filtermap(fun({Var, Index}) ->
        case lists:member(Index, Indexes) of
            true ->
                {true, {Index, Var}};
            false ->
                false
        end
    end, maps:get(vars, State)).

%%======================================================================
%% Internal functions
%%======================================================================

eval_parts(Partial, Indexes, Assigns, State, Opts) ->
    Parts = maps:get(parts, State),
    ToStringFun = maps:get(to_string, Opts, fun eel_converter:to_string/1),
    lists:foldl(fun(Index, Acc) ->
        Acc#{Index => eval_part(Partial, Index, Parts, ToStringFun, Assigns, State)}
    end, #{}, Indexes).

eval_part(Partial, Index, Parts, ToStringFun, Assigns, State) ->
    Bindings = #{
        '__PARTIAL__' => Partial,
        '__INDEX__' => Index,
        '__STATE__' => State,
        'Assigns' => Assigns
    },
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

%%======================================================================
%% Tests
%%======================================================================

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
                "<%% TODO: Items to binary. %%>"
                "<li><%= @item_prefix .%><%= integer_to_binary(Item) .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    Tokens = eel_tokenizer:tokenize(Bin),
    Tree = eel_structurer:tree(Tokens),

    ExpectedAll = #{1 => <<"EEl">>,
    3 =>
        [[<<"<li>">>,<<"Item - ">>,<<"1">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"2">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"3">>,<<"</li>">>]]},
    AssignsAll = #{title => <<"EEl">>, items => [1,2,3], item_prefix => <<"Item - ">>},
    StateAll = eel_compiler:compile(Tree),
    ResultAll = render(AssignsAll, StateAll),
    ?assertEqual(ExpectedAll, ResultAll),

    ExpectedChanges = #{1 => <<"EEl - Embedded Erlang">>},
    AssignsChanges = #{title => <<"EEl - Embedded Erlang">>},
    StateChanges = maps:merge(StateAll, ResultAll),
    ResultChanges = render_changes(AssignsChanges, StateChanges),
    ?assertEqual(ExpectedChanges, ResultChanges).

-endif.
