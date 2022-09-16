%%%---------------------------------------------------------------------------------------
%%% @doc EEl renderer module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%---------------------------------------------------------------------------------------
-module(eel_render).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    compiled/2,
    compiled/3
]).

-export_type([
    accepted_term/0,
    bindings/0,
    bindings_indexes/0,
    eval/0,
    memo/0,
    return/0
]).

-type accepted_term() ::
    binary()
    | atom()
    | float()
    | integer()
    | list()
    | tuple().
-type bindings() :: #{atom() => accepted_term()}.
-type bindings_indexes() :: #{integer() => accepted_term()}.
-type eval() :: term().
-type memo() :: #{eval => eval(), bindings => bindings()}.
-type return() :: {binary(), memo(), bindings_indexes()}.

%%------------------------------------------------------------------------------
%% @doc Render.
%% @end
%%------------------------------------------------------------------------------
-spec compiled(eel_compile:return(), bindings()) -> return().

compiled(Compiled, Bindings) ->
    compiled(Compiled, #{}, Bindings).

%%------------------------------------------------------------------------------
%% @doc Render passing a memo.
%% @end
%%------------------------------------------------------------------------------
-spec compiled(eel_compile:return(), memo(), bindings()) -> return().

compiled({Static, AST}, Memo0, NewBindings) ->
    Bindings = maps:merge(maps:get(bindings, Memo0, #{}), NewBindings),
    VarsToRender = maps:keys(NewBindings),
    EvalMemo = maps:get(eval, Memo0, []),
    {ReversedEval, _BindingsIndexes, Indexes, _} =
        lists:foldl(
            fun({Exprs, Vars}, {Acc, Indexes0, NewIndexes0, Index}) ->
                IsVarsValid =
                    case Vars of
                        [] -> true;
                        _ -> lists:any(fun(Var) -> lists:member(Var, VarsToRender) end, Vars)
                    end,
                {Bin, NewIndexes} =
                    case IsVarsValid of
                        true ->
                            {value, Result0, _} = erl_eval:exprs(Exprs, Bindings),
                            Result = eval_result(Result0, Bindings),
                            {Result, NewIndexes0#{Index => Result}};
                        false ->
                            case EvalMemo of
                                % TODO: Return custom error or maybe empty binary
                                [] -> erlang:error(badarg);
                                _ -> {lists:nth(Index, EvalMemo), NewIndexes0}
                            end
                    end,
                {[Bin | Acc], Indexes0#{Index => Bin}, NewIndexes, Index + 1}
            end,
            {[], #{}, #{}, 1},
            AST
        ),
    Eval = lists:reverse(ReversedEval),
    Rendered = merge(Static, Eval),
    Memo = #{
        eval => Eval,
        bindings => Bindings
    },
    {Rendered, Memo, Indexes}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

merge(Static, Dynamic) ->
    do_merge(Static, Dynamic, <<>>).

do_merge([S | Static], [D0 | Dynamic], Acc) ->
    D =
        case D0 of
            D0 when is_atom(D0) ->
                erlang:atom_to_binary(D0);
            D0 when is_binary(D0) ->
                D0
        end,
    do_merge(Static, Dynamic, <<Acc/binary, S/binary, D/binary>>);
do_merge([S | Static], [], Acc) ->
    do_merge(Static, [], <<Acc/binary, S/binary>>);
do_merge([], [], Acc) ->
    Acc;
do_merge([], Dynamic, Acc) ->
    erlang:iolist_to_binary([Acc, Dynamic]).

eval_result(Fun, Bindings) when is_function(Fun, 0) ->
    eval_result(Fun(), Bindings);
eval_result(Fun, Bindings) when is_function(Fun) ->
    {env, [Env]} = erlang:fun_info(Fun, env),
    AST = element(erlang:size(Env), Env),
    Str = erl_pp:expr({'fun', 1, {clauses, AST}}),
    Expr = erlang:iolist_to_binary([Str, "."]),
    {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Expr)),
    Vars = retrieve_vars(Tokens),
    Args = lists:map(fun(Key) -> maps:get(Key, Bindings) end, Vars),
    eval_result(erlang:apply(Fun, Args), Bindings);
eval_result(Bin, _Bindings) when is_binary(Bin) ->
    Bin;
eval_result(Result, _Bindings) ->
    % TODO: to_binary options
    eel_convert:to_binary(Result).

retrieve_vars(Tokens) ->
    do_retrieve_vars(Tokens, 1, []).

do_retrieve_vars(_Tokens, 0, Acc) ->
    Acc;
do_retrieve_vars([{'fun', _}, {'(', _} | Tokens], 1, Acc) ->
    do_retrieve_vars(Tokens, 1, Acc);
do_retrieve_vars([{')', _} | Tokens], Count, Acc) ->
    do_retrieve_vars(Tokens, Count - 1, Acc);
do_retrieve_vars([{'(', _} | Tokens], Count, Acc) ->
    do_retrieve_vars(Tokens, Count + 1, Acc);
do_retrieve_vars([{var, _, Var} | Tokens], Count, Acc) ->
    do_retrieve_vars(Tokens, Count, [Var | Acc]);
do_retrieve_vars([_ | Tokens], Count, Acc) ->
    do_retrieve_vars(Tokens, Count, Acc).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

compiled_test() ->
    Bin = <<
        "<h1><%= fun() -> fun(Title) -> Title end end .%></h1>"
        "<ul>"
        "<%= lists:map(fun(Item) -> %>"
        "<li><%= Item .%></li>"
        "<% end, List) .%>"
        "</ul>"
        "<%= Length = erlang:length(List), %>"
        "<div>Item count: <%= Length .%></div>"
        "<%= case erlang:length(List) > 0 of true -> %>"
        "<ul>"
        "<%= lists:map(fun(N) -> %>"
        "<li><%= N .%></li>"
        "<% end, lists:seq(1, erlang:length(List))) .%>"
        "</ul>"
        "<% ; false -> <<>> end .%>"
        "<% .%>"
        "<h2>Node: <%= erlang:node() .%></h2>"
    >>,
    Compiled = eel_compile:binary(Bin),
    Bindings = #{
        'Title' => <<"EEL">>,
        'List' => [<<"Foo">>, <<"Bar">>]
    },
    ExpectedRender =
        <<"<h1>EEL</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div><ul><li>1</li><li>2</li></ul><h2>Node: nonode@nohost</h2>">>,
    ExpectedIndexes = #{
        1 => <<"EEL">>,
        2 => <<"<li>Foo</li><li>Bar</li>">>,
        3 => <<"<div>Item count: 2</div><ul><li>1</li><li>2</li></ul>">>,
        4 => <<"nonode@nohost">>
    },
    {Render, Memo, Indexes} = compiled(Compiled, Bindings),
    ?assertEqual(ExpectedRender, Render),
    ?assertEqual(ExpectedIndexes, Indexes),

    NewBindings = #{
        'Title' => <<"Embedded Erlang">>
    },
    ExpectedMemoRender =
        <<"<h1>Embedded Erlang</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div><ul><li>1</li><li>2</li></ul><h2>Node: nonode@nohost</h2>">>,
    ExpectedMemoIndexes = #{
        1 => <<"Embedded Erlang">>,
        4 => <<"nonode@nohost">>
    },
    {MemoRender, _, MemoIndexes} = compiled(Compiled, Memo, NewBindings),
    ?assertEqual(ExpectedMemoRender, MemoRender),
    ?assertEqual(ExpectedMemoIndexes, MemoIndexes).

-endif.
