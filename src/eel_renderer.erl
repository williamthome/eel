-module(eel_renderer).

-export([ new_state/4
        , update_state_snapshot/2
        , update_changes_state_snapshot/2
        , render_state/2
        , render_state/3
        , render_state_changes/2
        , render_state_changes/3
        , render/3
        , render/4
        , render_changes/3
        , render_changes/4
        , get_vars_indexes/2
        , get_vars_from_indexes/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record( render_state,
       { parts
       , vars
       , dynamics
       , metadata
       , snapshot
       }).

%%======================================================================
%% API functions
%%======================================================================

new_state(Parts, Vars, Dynamics, Metadata) ->
    #render_state{
        parts = Parts,
        vars = Vars,
        dynamics = Dynamics,
        metadata = Metadata
    }.

update_state_snapshot(Snapshot, #render_state{parts = Parts} = State) ->
    State#render_state{snapshot = maps:merge(Parts, Snapshot)}.

update_changes_state_snapshot(Snapshot, #render_state{snapshot = Parts} = State) ->
    State#render_state{snapshot = maps:merge(Parts, Snapshot)}.

render_state(Assigns, State) ->
    render_state(Assigns, State, #{}).

render_state(Assigns, #render_state{dynamics = Indexes, parts = Parts}, Opts) ->
    render(Indexes, Assigns, Parts, Opts).

render_state_changes(Assigns, State) ->
    render_state_changes(Assigns, State, #{}).

render_state_changes(Assigns, #render_state{vars = Vars, parts = Parts}, Opts) ->
    render_changes(Assigns, Vars, Parts, Opts).

render(Indexes, Assigns, Parts) ->
    render(Indexes, Assigns, Parts, #{}).

render(Indexes, Assigns, Parts, Opts) ->
    ToStringFun = maps:get(to_string, Opts, fun eel_converter:to_string/1),
    case Opts of
        #{debug_info := true} ->
            Globals = maps:get(global_vars, Opts, #{}),
            Bindings0 = Globals#{
                'Assigns' => Assigns
            },
            lists:foldl(fun(Index, Acc) ->
                Bindings = Bindings0#{
                    '__INDEX__' => Index
                },
                Expr = maps:get(Index, Parts),
                Part = eval_expr(Expr, Bindings, ToStringFun),
                Acc#{Index => Part}
            end, #{}, Indexes);
        #{} ->
            Globals = maps:get(global_vars, Opts, #{}),
            Bindings = Globals#{'Assigns' => Assigns},
            lists:foldl(fun(Index, Acc) ->
                Expr = maps:get(Index, Parts),
                Part = eval_expr(Expr, Bindings, ToStringFun),
                Acc#{Index => Part}
            end, #{}, Indexes)
    end.

render_changes(Assigns, Vars, Parts) ->
    render_changes(Assigns, Vars, Parts, #{}).

render_changes(Assigns, Vars, Parts, Opts) ->
    Indexes = get_vars_indexes(Assigns, Vars),
    render(Indexes, Assigns, Parts, Opts).

get_vars_indexes(Assigns, #render_state{vars = Vars}) ->
    get_vars_indexes(Assigns, Vars);
get_vars_indexes(Assigns, Vars) ->
    sets:to_list(
        lists:foldl(fun(Var, Set0) ->
            Indexes = proplists:get_all_values(Var, Vars),
            lists:foldl(fun(Index, Set) ->
                sets:add_element(Index, Set)
            end, Set0, Indexes)
        end, sets:new([{version, 2}]), maps:keys(Assigns))
    ).

get_vars_from_indexes(Indexes, #render_state{vars = Vars}) ->
    get_vars_from_indexes(Indexes, Vars);
get_vars_from_indexes(Indexes, Vars) ->
    lists:filtermap(fun({Var, Index}) ->
        case lists:member(Index, Indexes) of
            true ->
                {true, {Index, Var}};
            false ->
                false
        end
    end, Vars).

%%======================================================================
%% Internal functions
%%======================================================================

eval_expr(Expr, Bindings, ToStringFun) ->
    {value, Term, _} = erl_eval:exprs(Expr, Bindings),
    to_string(Term, ToStringFun).

% TODO: Let the user transform data to string.
to_string(IOData, _ToStringFun) when is_binary(IOData) ->
    IOData;
to_string([IOData], _ToStringFun) when is_binary(IOData) ->
    IOData;
to_string([Term], ToStringFun) ->
    ToStringFun(Term);
to_string(IOList, ToStringFun) when is_list(IOList) ->
    % NOTE: Here we have a list that can be optimized to the changes return.
    %       We need to know the static x dynamics of the expression.
    % ?debugFmt("[TODO: Optimize list] ~p~n", [{Index, IOList}]),
    lists:map(fun(Term) -> to_string(Term, ToStringFun) end, IOList);
to_string(Term, ToStringFun) ->
    ToStringFun(Term).

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
                "<li><%= @item_prefix .%><%= Item .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    Tokens = eel_tokenizer:tokenize(Bin),
    Tree = eel_structurer:tree(Tokens),
    State = eel_compiler:compile(Tree),

    RenderExpected = #{1 => <<"EEl">>,
    3 =>
        [[<<"<li>">>,<<"Item - ">>,<<"1">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"2">>,<<"</li>">>],
         [<<"<li>">>,<<"Item - ">>,<<"3">>,<<"</li>">>]]},
    AssignsAll = #{title => <<"EEl">>, items => [1,2,3], item_prefix => <<"Item - ">>},
    RenderSnapshot = render_state(AssignsAll, State),
    RenderState = update_state_snapshot(RenderSnapshot, State),
    ?assertEqual(RenderExpected, RenderSnapshot),

    ChangesExpected = #{1 => <<"EEl - Embedded Erlang">>},
    AssignsChanges = #{title => <<"EEl - Embedded Erlang">>},
    ChangesSnapshot = render_state_changes(AssignsChanges, RenderState),
    ?assertEqual(ChangesExpected, ChangesSnapshot).

-endif.
