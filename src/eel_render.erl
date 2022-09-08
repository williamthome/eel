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
                {Bin, NewIndexes} =
                    case lists:any(fun(Var) -> lists:member(Var, VarsToRender) end, Vars) of
                        true ->
                            {value, Result0, _} = erl_eval:exprs(Exprs, Bindings),
                            % TODO: to_binary options
                            Result = eel_convert:to_binary(Result0),
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

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

compiled_test() ->
    Bin = <<
        "<h1><%= Title .%></h1>"
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
    >>,
    Compiled = eel_compile:binary(Bin),
    Bindings = #{
        'Title' => <<"EEL">>,
        'List' => [<<"Foo">>, <<"Bar">>]
    },
    ExpectedRender =
        <<"<h1>EEL</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div><ul><li>1</li><li>2</li></ul>">>,
    ExpectedIndexes = #{
        1 => <<"EEL">>,
        2 => <<"<li>Foo</li><li>Bar</li>">>,
        3 => <<"<div>Item count: 2</div><ul><li>1</li><li>2</li></ul>">>
    },
    {Render, Memo, Indexes} = compiled(Compiled, Bindings),
    ?assertEqual(ExpectedRender, Render),
    ?assertEqual(ExpectedIndexes, Indexes),

    NewBindings =
        Bindings = #{
            'Title' => <<"Embedded Erlang">>
        },
    ExpectedMemoRender =
        <<"<h1>Embedded Erlang</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div><ul><li>1</li><li>2</li></ul>">>,
    ExpectedMemoIndexes = #{
        1 => <<"Embedded Erlang">>
    },
    {MemoRender, _, MemoIndexes} = compiled(Compiled, Memo, NewBindings),
    ?assertEqual(ExpectedMemoRender, MemoRender),
    ?assertEqual(ExpectedMemoIndexes, MemoIndexes).

-endif.