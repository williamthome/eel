%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023-2024 William Fank Thomé
%% @doc Renderer.

%% Copyright 2023-2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(eel_renderer).

%% API
-export([ new_state/3
        , get_state_parts/1
        , get_state_vars/1
        , get_state_dynamics/1
        , get_state_snapshot/1
        , update_state_snapshot/2
        , update_changes_state_snapshot/2
        , state_snapshot_to_iolist/1
        , state_snapshot_to_binary/1
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
        , to_string/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record( render_state,
       { parts
       , vars
       , dynamics
       , snapshot
       }).

%%%=====================================================================
%%% API
%%%=====================================================================

new_state(Parts, Vars, Dynamics) ->
    #render_state{
        parts = Parts,
        vars = Vars,
        dynamics = Dynamics
    }.

get_state_parts(#render_state{parts = Parts}) ->
    Parts.

get_state_vars(#render_state{vars = Vars}) ->
    Vars.

get_state_dynamics(#render_state{dynamics = Dynamics}) ->
    Dynamics.

get_state_snapshot(#render_state{snapshot = Snapshot}) ->
    Snapshot.

update_state_snapshot(Snapshot, #render_state{parts = Parts} = State) ->
    update_snapshot(Snapshot, Parts, State).

update_changes_state_snapshot(Snapshot, #render_state{snapshot = Parts} = State) ->
    update_snapshot(Snapshot, Parts, State).

state_snapshot_to_iolist(#render_state{snapshot = Snapshot}) ->
    [IOData || {_, IOData} <- Snapshot].

state_snapshot_to_binary(#render_state{snapshot = Snapshot}) ->
    << IOData || {_, IOData} <- Snapshot >>.

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
    LocalFunHandler = maps:get(local_function_handler, Opts, none),
    NonLocalFunHandler = maps:get(non_local_function_handler, Opts, none),
    ToStringFun = maps:get(to_string, Opts, fun to_string/1),
    case Opts of
        #{debug_info := true} ->
            Globals = maps:get(global_vars, Opts, #{}),
            Bindings0 = Globals#{
                'Assigns' => Assigns
            },
            lists:reverse(lists:foldl(fun(Index, Acc) ->
                Bindings = Bindings0#{
                    '__INDEX__' => Index
                },
                {Index, Expr} = proplists:lookup(Index, Parts),
                Part = eval_expr( Expr
                                , Bindings
                                , LocalFunHandler
                                , NonLocalFunHandler
                                , ToStringFun
                                ),
                [{Index, Part} | Acc]
            end, [], Indexes));
        #{} ->
            Globals = maps:get(global_vars, Opts, #{}),
            Bindings = Globals#{'Assigns' => Assigns},
            lists:reverse(lists:foldl(fun(Index, Acc) ->
                {Index, Expr} = proplists:lookup(Index, Parts),
                Part = eval_expr( Expr
                                , Bindings
                                , LocalFunHandler
                                , NonLocalFunHandler
                                , ToStringFun
                                ),
                [{Index, Part} | Acc]
            end, [], Indexes))
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

to_string(IOData) when is_binary(IOData) ->
    IOData;
to_string([IOData]) when is_binary(IOData) ->
    IOData;
to_string([Term]) ->
    eel_converter:to_string(Term);
to_string(IOList) when is_list(IOList) ->
    lists:map(fun(Term) -> to_string(Term) end, IOList);
to_string(Term) ->
    eel_converter:to_string(Term).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

update_snapshot(Snapshot0, Parts, State) ->
    Snapshot = lists:foldl(fun({K, V}, Acc) ->
        lists:keyreplace(K, 1, Acc, {K, V})
    end, Parts, Snapshot0),
    State#render_state{snapshot = Snapshot}.

eval_expr(Expr, Bindings, LocalFunHandler, NonLocalFunHandler, ToStringFun) ->
    {value, Value, _NewBindings} = erl_eval:exprs( Expr
                                                 , Bindings
                                                 , LocalFunHandler
                                                 , NonLocalFunHandler
                                                 ),
    ToStringFun(Value).

%%%=====================================================================
%%% Test
%%%=====================================================================

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
    TokenizeState = eel_tokenizer:tokenize(Bin),
    Tokens = eel_tokenizer:get_tokens(TokenizeState),
    Tree = eel_structurer:tree(Tokens),
    RenderState0 = eel_compiler:compile(Tree),

    RenderExpected = [{2,<<"EEl">>},
    {4,
     [[<<"<li>">>,<<"Item - ">>,<<"1">>,<<"</li>">>],
      [<<"<li>">>,<<"Item - ">>,<<"2">>,<<"</li>">>],
      [<<"<li>">>,<<"Item - ">>,<<"3">>,<<"</li>">>]]}],
    AssignsAll = #{title => <<"EEl">>, items => [1,2,3], item_prefix => <<"Item - ">>},
    RenderSnapshot = render_state(AssignsAll, RenderState0),
    RenderState = update_state_snapshot(RenderSnapshot, RenderState0),
    ?assertEqual(RenderExpected, RenderSnapshot),

    ChangesExpected = [{2,<<"EEl - Embedded Erlang">>}],
    AssignsChanges = #{title => <<"EEl - Embedded Erlang">>},
    ChangesSnapshot = render_state_changes(AssignsChanges, RenderState),
    ?assertEqual(ChangesExpected, ChangesSnapshot).

-endif.
