%%%-----------------------------------------------------------------------------
%%% @doc Embedded Erlang library.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-export([
    compile/1,
    compile_file/1,
    compile_file/2,
    render/3,
    render/4
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(RE_WS_TRIM, re:compile(<<"^\\s+|\\s+$">>)).

compile(Bin) ->
    {Static, Dynamic} = tokenize(Bin),
    Flattened = flatten(Dynamic),
    AST = parse(Flattened),
    {Static, AST}.

compile_file(App, FileName0) ->
    PrivDir = code:priv_dir(App),
    FileName = filename:join([PrivDir, "templates", FileName0]),
    compile_file(FileName).

compile_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> compile(Bin);
        {error, Reason} -> {error, Reason}
    end.

render(Static, AST, Bindings) ->
    render(Static, AST, #{}, Bindings).

render(Static, AST, Memo, NewBindings) ->
    Bindings = maps:merge(maps:get(bindings, Memo, #{}), NewBindings),
    VarsToRender = maps:keys(NewBindings),
    EvalMemo = maps:get(eval, Memo, []),
    {ReversedEval, BindingsIndexes, NewBindingsIndexes, _} =
        lists:foldl(
            fun({Exprs, Vars}, {Acc, Indexes0, NewIndexes0, Index}) ->
                {Bin, NewIndexes} =
                    case lists:any(fun(Var) -> lists:member(Var, VarsToRender) end, Vars) of
                        true ->
                            {value, Result0, _} = erl_eval:exprs(Exprs, Bindings),
                            % TODO: to_binary options
                            Result = eel_utils:to_binary(Result0),
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
    Render = merge(Static, Eval),
    NewMemo = #{
        eval => Eval,
        bindings => Bindings
    },
    % TODO: Remove Static from the bindings tuple
    {Render, NewMemo, {Static, BindingsIndexes, NewBindingsIndexes}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

tokenize(Bin) ->
    do_tokenize(Bin, {[], []}, [], <<>>).

do_tokenize(<<"<%", _/binary>> = Bin, {Static0, Dynamic0}, TokensAcc, Acc0) ->
    case tokenize_expr(Bin, Acc0) of
        {ok, {ExprRef, Token, Rest, Acc}} ->
            Static =
                case TokensAcc of
                    [{ExprRef, _} | _] when ExprRef =:= expr; ExprRef =:= start_expr ->
                        [<<>> | Static0];
                    _ ->
                        Static0
                end,
            Dynamic1 =
                case lists:member(ExprRef, [expr, start_expr]) of
                    true ->
                        [[Token] | Dynamic0];
                    false ->
                        [DH0 | DT0] = Dynamic0,
                        [[Token | DH0] | DT0]
                end,
            Dynamic =
                case ExprRef =:= end_expr of
                    true ->
                        [DH1 | DT1] = Dynamic1,
                        [lists:reverse(DH1) | DT1];
                    false ->
                        Dynamic1
                end,
            do_tokenize(Rest, {Static, Dynamic}, [Token | TokensAcc], Acc);
        {ok, {_Comment, Rest, _Acc}} ->
            % TODO: Maybe the comment supress can be optional
            % Token = {comment, Comment},
            % Dynamic = [Token | Dynamic0],
            do_tokenize(Rest, {Static0, Dynamic0}, TokensAcc, Acc0);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(Bin, {Static, [HD | Dynamic]}, [{ExprRef, _Expr} | _] = TokensAcc, Acc) when
    ExprRef =:= start_expr; ExprRef =:= mid_expr
->
    case nested(Bin, 0, <<>>) of
        {ok, {Nested, T}} ->
            Token = {nested_expr, tokenize(Nested)},
            do_tokenize(
                T,
                {Static, [[Token | HD] | Dynamic]},
                [Token | TokensAcc],
                <<Acc/binary, Nested/binary>>
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<H, T/binary>>, {Static0, Dynamic}, TokensAcc0, Acc) ->
    {TokenStatic, Static} =
        case Static0 of
            [] ->
                {<<H>>, [<<H>>]};
            [SH | ST] ->
                case TokensAcc0 of
                    [{text, _} | _] -> {<<SH/binary, H>>, [<<SH/binary, H>> | ST]};
                    _ -> {<<H>>, [<<H>>, SH | ST]}
                end
        end,
    Token = {text, TokenStatic},
    TokensAcc =
        case TokensAcc0 of
            [{text, _} | TT] -> [Token | TT];
            _ -> [Token | TokensAcc0]
        end,
    do_tokenize(T, {Static, Dynamic}, TokensAcc, <<Acc/binary, H>>);
do_tokenize(<<>>, {Static, Dynamic}, _TokensAcc, _Acc) ->
    {lists:reverse(Static), lists:reverse(Dynamic)}.

tokenize_expr(<<"<%", T0/binary>>, Acc0) ->
    {StartMarker, MaybeSpace, T} = retrieve_marker(T0, <<>>),
    Acc1 = <<Acc0/binary, "<%", StartMarker/binary, MaybeSpace/binary>>,
    tokenize_by_marker(StartMarker, T, Acc1).

tokenize_by_marker(<<"#">>, Bin, Acc0) ->
    comment(Bin, <<>>, Acc0);
tokenize_by_marker(StartMarker, Bin, Acc1) ->
    case expression(Bin, StartMarker, <<>>, Acc1) of
        {ok, {ExprRef, Expr, EndMarker, Rest, Acc}} ->
            Token = {ExprRef, {{StartMarker, EndMarker}, Expr}},
            {ok, {ExprRef, Token, Rest, Acc}};
        {error, Reason} ->
            {error, Reason}
    end.

comment(<<32, "#%>", T/binary>>, Cache, Acc0) ->
    Comment = trim(Cache),
    {ok, {Comment, T, <<Acc0/binary, Comment/binary, " #%>">>}};
comment(<<"#%>", _/binary>>, _Cache, _Acc) ->
    % TODO: Handle missing_space
    {error, missing_space};
comment(<<H, T/binary>>, Cache, Acc) ->
    comment(T, <<Cache/binary, H>>, Acc);
comment(<<>>, _Cache, _Acc) ->
    {error, eof}.

expression(<<32, "%>", T/binary>>, StartMarker, Cache, Acc) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> mid_expr;
            _ -> start_expr
        end,
    {ok, {ExprRef, Expr, <<32>>, T, <<Acc/binary, Expr/binary, " %>">>}};
expression(<<32, EndMarker, "%>", T/binary>>, StartMarker, Cache, Acc) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> end_expr;
            _ -> expr
        end,
    {ok, {ExprRef, Expr, <<EndMarker>>, T, <<Acc/binary, Expr/binary, 32, EndMarker, "%>">>}};
expression(<<"<%", _/binary>>, _StartMarker, _Cache, _Acc) ->
    % TODO: Handle unknown start marker
    {error, unknown_start_marker};
expression(<<"%>", T/binary>>, _StartMarker, Cache, Acc) ->
    case is_cache_empty(Cache) of
        true ->
            {ok, {end_expr, <<>>, <<".">>, T, <<Acc/binary, 32, ".%>">>}};
        false ->
            % TODO: Handle unknown end marker
            {error, unknown_end_marker}
    end;
expression(<<H, T/binary>>, StartMarker, Cache, Acc) ->
    expression(T, StartMarker, <<Cache/binary, H>>, Acc);
expression(<<>>, _StartMarker, _Cache, _Acc) ->
    {error, eof}.

is_cache_empty(<<".">>) ->
    true;
is_cache_empty(<<32, T/binary>>) ->
    is_cache_empty(T);
is_cache_empty(<<_, _/binary>>) ->
    false;
is_cache_empty(<<>>) ->
    true.

nested(<<"<% ", _/binary>> = T, 0, Expr) ->
    {ok, {Expr, T}};
nested(<<"<%= ", T/binary>>, ExprCount, Expr) ->
    nested(T, ExprCount + 1, <<Expr/binary, "<%= ">>);
nested(<<" .%>", T/binary>>, ExprCount, Expr) ->
    nested(T, ExprCount - 1, <<Expr/binary, " .%>">>);
nested(<<H, T/binary>>, ExprCount, Expr) ->
    nested(T, ExprCount, <<Expr/binary, H>>);
nested(<<>>, _ExprCount, _Expr) ->
    {error, eof}.

retrieve_marker(<<32, T/binary>>, <<>>) ->
    {<<32>>, <<>>, T};
retrieve_marker(<<32, _/binary>> = T, Marker) ->
    {Marker, <<32>>, T};
retrieve_marker(<<H, T/binary>>, Marker) ->
    retrieve_marker(T, <<Marker/binary, H>>);
retrieve_marker(<<>>, _Marker) ->
    {error, eof}.

trim(Bin) ->
    {ok, RE} = ?RE_WS_TRIM,
    re:replace(Bin, RE, "", [{return, binary}, global]).

retrieve_vars(Expr0) ->
    Expr = erlang:binary_to_list(Expr0),
    {ok, Tokens, _} = erl_scan:string(Expr),
    do_retrieve_vars(Tokens, [], []).

do_retrieve_vars([{'fun', _}, {'(', _} | Tokens0], LocalVars0, Acc) ->
    {Tokens, LocalVars1} = ignore_fun_vars(Tokens0, 1, LocalVars0),
    LocalVars = lists:merge(LocalVars0, LocalVars1),
    do_retrieve_vars(Tokens, LocalVars, Acc);
do_retrieve_vars([{var, _, Var}, {'=', _} | Tokens], LocalVars, Acc) ->
    do_retrieve_vars(Tokens, [Var, LocalVars], Acc);
do_retrieve_vars([{':=', _}, {var, _, Var} | Tokens], LocalVars, Acc) ->
    do_retrieve_vars(Tokens, [Var, LocalVars], Acc);
do_retrieve_vars([{var, _, Var} | Tokens], LocalVars, Acc0) ->
    Acc =
        case lists:member(Var, LocalVars) of
            true -> Acc0;
            false -> [Var | Acc0]
        end,
    do_retrieve_vars(Tokens, LocalVars, Acc);
do_retrieve_vars([_Token | Tokens], LocalVars, Acc) ->
    do_retrieve_vars(Tokens, LocalVars, Acc);
do_retrieve_vars([], _LocalVars, Acc) ->
    lists:reverse(Acc).

ignore_fun_vars(Tokens, 0, LocalVars) ->
    {Tokens, LocalVars};
ignore_fun_vars([{')', _} | Tokens], Count, LocalVars) ->
    ignore_fun_vars(Tokens, Count - 1, LocalVars);
ignore_fun_vars([{'(', _} | Tokens], Count, LocalVars) ->
    ignore_fun_vars(Tokens, Count + 1, LocalVars);
ignore_fun_vars([{var, _, Var} | Tokens], Count, LocalVars) ->
    ignore_fun_vars(Tokens, Count, [Var | LocalVars]);
ignore_fun_vars([_ | Tokens], Count, LocalVars) ->
    ignore_fun_vars(Tokens, Count, LocalVars).

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

flatten(Dynamic) ->
    lists:map(fun merge_expression/1, Dynamic).

merge_expression(Expression) ->
    Merged =
        lists:foldl(
            fun
                ({nested_expr, {Static, Dynamic}}, Acc) ->
                    NestedExpression = nested_expression(Static, Dynamic),
                    <<Acc/binary, NestedExpression/binary, 32>>;
                ({ERef, {_, Expr}}, Acc) ->
                    MaybeSpace =
                        case lists:member(ERef, [expr, end_expr]) of
                            true -> <<>>;
                            false -> <<32>>
                        end,
                    <<Acc/binary, Expr/binary, MaybeSpace/binary>>
            end,
            <<>>,
            Expression
        ),
    <<Merged/binary, $.>>.

extract_dynamic_expression(Dynamic) ->
    do_extract_dynamic_expression(Dynamic, []).

do_extract_dynamic_expression([Token | Dynamic], Acc) ->
    Expression = extract_token_expression(Token, Acc),
    do_extract_dynamic_expression(Dynamic, [Expression | Acc]);
do_extract_dynamic_expression([], Acc) ->
    lists:reverse(Acc).

extract_token_expression([{nested_expr, {Static, Dynamic}} | Tokens], Acc) ->
    NestedExpression = nested_expression(Static, Dynamic),
    extract_token_expression(Tokens, [{nested_expr, NestedExpression} | Acc]);
extract_token_expression([{ExprRef, {_, Expr}} | Tokens], Acc) ->
    extract_token_expression(Tokens, [{ExprRef, Expr} | Acc]);
extract_token_expression([], Acc0) ->
    % TODO: Check why the filtermap is needed
    Acc = lists:join(
        " ",
        lists:reverse(
            lists:filtermap(
                fun
                    ({_, Expr}) -> {true, Expr};
                    (_) -> false
                end,
                Acc0
            )
        )
    ),
    erlang:iolist_to_binary([
        "eel_utils:to_binary(begin ", Acc, " end)"
    ]).

nested_expression(Static, Dynamic) ->
    DynamicExpr = extract_dynamic_expression(Dynamic),
    {IOList, Siblings} = join(Static, DynamicExpr),
    iolist_to_binary_fun(IOList, Siblings).

join(Static, Dynamic) ->
    do_join(Static, Dynamic, [], []).

do_join([S | Static], [D | Dynamic], IOList, Siblings) ->
    do_join(Static, Dynamic, [D, S | IOList], ["~s", "~p" | Siblings]);
do_join([S | Static], [], IOList, Siblings) ->
    do_join(Static, [], [S | IOList], ["~p" | Siblings]);
do_join([], _, IOList, Siblings) ->
    {lists:reverse(IOList), lists:reverse(Siblings)}.

iolist_to_binary_fun(IOList, Siblings) ->
    Format = erlang:iolist_to_binary([
        "erlang:iolist_to_binary([", lists:join(", ", Siblings), "])"
    ]),
    List = io_lib:format(Format, IOList),
    erlang:list_to_binary(List).

% TODO: Return ok or error tuple.
parse(Flattened) ->
    lists:map(
        fun(Expr) ->
            Vars = retrieve_vars(Expr),
            {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Expr)),
            {ok, Exprs} = erl_parse:parse_exprs(Tokens),
            {Exprs, Vars}
        end,
        Flattened
    ).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
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
    Expected = {[<<"<h1>">>, <<"</h1><ul>">>, <<"</ul>">>], [
        [{expr, {{<<"=">>, <<".">>}, <<"Title">>}}],
        [
            {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(Item) ->">>}},
            {nested_expr,
                {[<<"<li>">>, <<"</li>">>], [[{expr, {{<<"=">>, <<".">>}, <<"Item">>}}]]}},
            {end_expr, {{<<" ">>, <<".">>}, <<"end, List)">>}}
        ],
        [
            {start_expr, {{<<"=">>, <<" ">>}, <<"Length = erlang:length(List),">>}},
            {nested_expr,
                {[<<"<div>Item count: ">>, <<"</div>">>], [
                    [{expr, {{<<"=">>, <<".">>}, <<"Length">>}}],
                    [
                        {start_expr, {
                            {<<"=">>, <<" ">>}, <<"case erlang:length(List) > 0 of true ->">>
                        }},
                        {nested_expr,
                            {[<<"<ul>">>, <<"</ul>">>], [
                                [
                                    {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(N) ->">>}},
                                    {nested_expr,
                                        {[<<"<li>">>, <<"</li>">>], [
                                            [{expr, {{<<"=">>, <<".">>}, <<"N">>}}]
                                        ]}},
                                    {end_expr, {
                                        {<<" ">>, <<".">>},
                                        <<"end, lists:seq(1, erlang:length(List)))">>
                                    }}
                                ]
                            ]}},
                        {end_expr, {{<<" ">>, <<".">>}, <<"; false -> <<>> end">>}}
                    ]
                ]}},
            {end_expr, {{<<" ">>, <<".">>}, <<>>}}
        ]
    ]},
    ?assertEqual(Expected, tokenize(Bin)).

flatten_test() ->
    Dynamic = [
        [{expr, {{<<"=">>, <<".">>}, <<"Title">>}}],
        [
            {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(Item) ->">>}},
            {nested_expr,
                {[<<"<li>">>, <<"</li>">>], [[{expr, {{<<"=">>, <<".">>}, <<"Item">>}}]]}},
            {end_expr, {{<<" ">>, <<".">>}, <<"end, List)">>}}
        ],
        [
            {start_expr, {{<<"=">>, <<" ">>}, <<"Length = erlang:length(List),">>}},
            {nested_expr,
                {[<<"<div>Item count: ">>, <<"</div>">>], [
                    [{expr, {{<<"=">>, <<".">>}, <<"Length">>}}],
                    [
                        {start_expr, {
                            {<<"=">>, <<" ">>}, <<"case erlang:length(List) > 0 of true ->">>
                        }},
                        {nested_expr,
                            {[<<"<ul>">>, <<"</ul>">>], [
                                [
                                    {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(N) ->">>}},
                                    {nested_expr,
                                        {[<<"<li>">>, <<"</li>">>], [
                                            [{expr, {{<<"=">>, <<".">>}, <<"N">>}}]
                                        ]}},
                                    {end_expr, {
                                        {<<" ">>, <<".">>},
                                        <<"end, lists:seq(1, erlang:length(List)))">>
                                    }}
                                ]
                            ]}},
                        {end_expr, {{<<" ">>, <<".">>}, <<"; false -> <<>> end">>}}
                    ]
                ]}},
            {end_expr, {{<<" ">>, <<".">>}, <<>>}}
        ]
    ],
    Expected = [
        <<"Title.">>,
        <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>,
        <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_utils:to_binary(begin Length end), <<\"</div>\">>, eel_utils:to_binary(begin case erlang:length(List) > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_utils:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, erlang:length(List))) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
    ],
    ?assertEqual(Expected, flatten(Dynamic)).

retrieve_vars_test() ->
    [
        ?assertEqual(
            ['Title'],
            retrieve_vars(<<"Title.">>)
        ),
        ?assertEqual(
            ['List'],
            retrieve_vars(
                <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>
            )
        ),
        ?assertEqual(
            ['List', 'List', 'List'],
            retrieve_vars(
                <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_utils:to_binary(begin Length end), <<\"</div>\">>, eel_utils:to_binary(begin case erlang:length(List) > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_utils:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, erlang:length(List))) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
            )
        )
    ].

parse_test() ->
    Flattened = [
        <<"Title.">>,
        <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>,
        <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_utils:to_binary(begin Length end), <<\"</div>\">>, eel_utils:to_binary(begin case erlang:length(List) > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_utils:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, erlang:length(List))) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
    ],
    Expected = [
        {[{var, 1, 'Title'}], ['Title']},
        {
            [
                {call, 1, {remote, 1, {atom, 1, lists}, {atom, 1, map}}, [
                    {'fun', 1,
                        {clauses, [
                            {clause, 1, [{var, 1, 'Item'}], [], [
                                {call, 1,
                                    {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                                        {cons, 1,
                                            {bin, 1, [
                                                {bin_element, 1, {string, 1, "<li>"}, default,
                                                    default}
                                            ]},
                                            {cons, 1,
                                                {call, 1,
                                                    {remote, 1, {atom, 1, eel_utils},
                                                        {atom, 1, to_binary}},
                                                    [{block, 1, [{var, 1, 'Item'}]}]},
                                                {cons, 1,
                                                    {bin, 1, [
                                                        {bin_element, 1, {string, 1, "</li>"},
                                                            default, default}
                                                    ]},
                                                    {nil, 1}}}}
                                    ]}
                            ]}
                        ]}},
                    {var, 1, 'List'}
                ]}
            ],
            ['List']
        },
        {
            [
                {match, 1, {var, 1, 'Length'},
                    {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, length}}, [{var, 1, 'List'}]}},
                {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                    {cons, 1,
                        {bin, 1, [
                            {bin_element, 1, {string, 1, "<div>Item count: "}, default, default}
                        ]},
                        {cons, 1,
                            {call, 1, {remote, 1, {atom, 1, eel_utils}, {atom, 1, to_binary}}, [
                                {block, 1, [{var, 1, 'Length'}]}
                            ]},
                            {cons, 1,
                                {bin, 1, [
                                    {bin_element, 1, {string, 1, "</div>"}, default, default}
                                ]},
                                {cons, 1,
                                    {call, 1,
                                        {remote, 1, {atom, 1, eel_utils}, {atom, 1, to_binary}}, [
                                            {block, 1, [
                                                {'case', 1,
                                                    {op, 1, '>',
                                                        {call, 1,
                                                            {remote, 1, {atom, 1, erlang},
                                                                {atom, 1, length}},
                                                            [{var, 1, 'List'}]},
                                                        {integer, 1, 0}},
                                                    [
                                                        {clause, 1, [{atom, 1, true}], [], [
                                                            {call, 1,
                                                                {remote, 1, {atom, 1, erlang},
                                                                    {atom, 1, iolist_to_binary}},
                                                                [
                                                                    {cons, 1,
                                                                        {bin, 1, [
                                                                            {bin_element, 1,
                                                                                {string, 1, "<ul>"},
                                                                                default, default}
                                                                        ]},
                                                                        {cons, 1,
                                                                            {call, 1,
                                                                                {remote, 1,
                                                                                    {atom, 1,
                                                                                        eel_utils},
                                                                                    {atom, 1,
                                                                                        to_binary}},
                                                                                [
                                                                                    {block, 1, [
                                                                                        {call, 1,
                                                                                            {remote,
                                                                                                1,
                                                                                                {atom,
                                                                                                    1,
                                                                                                    lists},
                                                                                                {atom,
                                                                                                    1,
                                                                                                    map}},
                                                                                            [
                                                                                                {'fun',
                                                                                                    1,
                                                                                                    {clauses,
                                                                                                        [
                                                                                                            {clause,
                                                                                                                1,
                                                                                                                [
                                                                                                                    {var,
                                                                                                                        1,
                                                                                                                        'N'}
                                                                                                                ],
                                                                                                                [],
                                                                                                                [
                                                                                                                    {call,
                                                                                                                        1,
                                                                                                                        {remote,
                                                                                                                            1,
                                                                                                                            {atom,
                                                                                                                                1,
                                                                                                                                erlang},
                                                                                                                            {atom,
                                                                                                                                1,
                                                                                                                                iolist_to_binary}},
                                                                                                                        [
                                                                                                                            {cons,
                                                                                                                                1,
                                                                                                                                {bin,
                                                                                                                                    1,
                                                                                                                                    [
                                                                                                                                        {bin_element,
                                                                                                                                            1,
                                                                                                                                            {string,
                                                                                                                                                1,
                                                                                                                                                "<li>"},
                                                                                                                                            default,
                                                                                                                                            default}
                                                                                                                                    ]},
                                                                                                                                {cons,
                                                                                                                                    1,
                                                                                                                                    {call,
                                                                                                                                        1,
                                                                                                                                        {remote,
                                                                                                                                            1,
                                                                                                                                            {atom,
                                                                                                                                                1,
                                                                                                                                                eel_utils},
                                                                                                                                            {atom,
                                                                                                                                                1,
                                                                                                                                                to_binary}},
                                                                                                                                        [
                                                                                                                                            {block,
                                                                                                                                                1,
                                                                                                                                                [
                                                                                                                                                    {var,
                                                                                                                                                        1,
                                                                                                                                                        'N'}
                                                                                                                                                ]}
                                                                                                                                        ]},
                                                                                                                                    {cons,
                                                                                                                                        1,
                                                                                                                                        {bin,
                                                                                                                                            1,
                                                                                                                                            [
                                                                                                                                                {bin_element,
                                                                                                                                                    1,
                                                                                                                                                    {string,
                                                                                                                                                        1,
                                                                                                                                                        "</li>"},
                                                                                                                                                    default,
                                                                                                                                                    default}
                                                                                                                                            ]},
                                                                                                                                        {nil,
                                                                                                                                            1}}}}
                                                                                                                        ]}
                                                                                                                ]}
                                                                                                        ]}},
                                                                                                {call,
                                                                                                    1,
                                                                                                    {remote,
                                                                                                        1,
                                                                                                        {atom,
                                                                                                            1,
                                                                                                            lists},
                                                                                                        {atom,
                                                                                                            1,
                                                                                                            seq}},
                                                                                                    [
                                                                                                        {integer,
                                                                                                            1,
                                                                                                            1},
                                                                                                        {call,
                                                                                                            1,
                                                                                                            {remote,
                                                                                                                1,
                                                                                                                {atom,
                                                                                                                    1,
                                                                                                                    erlang},
                                                                                                                {atom,
                                                                                                                    1,
                                                                                                                    length}},
                                                                                                            [
                                                                                                                {var,
                                                                                                                    1,
                                                                                                                    'List'}
                                                                                                            ]}
                                                                                                    ]}
                                                                                            ]}
                                                                                    ]}
                                                                                ]},
                                                                            {cons, 1,
                                                                                {bin, 1, [
                                                                                    {bin_element, 1,
                                                                                        {string, 1,
                                                                                            "</ul>"},
                                                                                        default,
                                                                                        default}
                                                                                ]},
                                                                                {nil, 1}}}}
                                                                ]}
                                                        ]},
                                                        {clause, 1, [{atom, 1, false}], [], [
                                                            {bin, 1, []}
                                                        ]}
                                                    ]}
                                            ]}
                                        ]},
                                    {nil, 1}}}}}
                ]}
            ],
            ['List', 'List', 'List']
        }
    ],
    ?assertEqual(Expected, parse(Flattened)).

compile_test() ->
    Bin =
        <<
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
    Expected = {[<<"<h1>">>, <<"</h1><ul>">>, <<"</ul>">>], [
        {[{var, 1, 'Title'}], ['Title']},
        {
            [
                {call, 1, {remote, 1, {atom, 1, lists}, {atom, 1, map}}, [
                    {'fun', 1,
                        {clauses, [
                            {clause, 1, [{var, 1, 'Item'}], [], [
                                {call, 1,
                                    {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                                        {cons, 1,
                                            {bin, 1, [
                                                {bin_element, 1, {string, 1, "<li>"}, default,
                                                    default}
                                            ]},
                                            {cons, 1,
                                                {call, 1,
                                                    {remote, 1, {atom, 1, eel_utils},
                                                        {atom, 1, to_binary}},
                                                    [{block, 1, [{var, 1, 'Item'}]}]},
                                                {cons, 1,
                                                    {bin, 1, [
                                                        {bin_element, 1, {string, 1, "</li>"},
                                                            default, default}
                                                    ]},
                                                    {nil, 1}}}}
                                    ]}
                            ]}
                        ]}},
                    {var, 1, 'List'}
                ]}
            ],
            ['List']
        },
        {
            [
                {match, 1, {var, 1, 'Length'},
                    {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, length}}, [
                        {var, 1, 'List'}
                    ]}},
                {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                    {cons, 1,
                        {bin, 1, [
                            {bin_element, 1, {string, 1, "<div>Item count: "}, default, default}
                        ]},
                        {cons, 1,
                            {call, 1, {remote, 1, {atom, 1, eel_utils}, {atom, 1, to_binary}}, [
                                {block, 1, [{var, 1, 'Length'}]}
                            ]},
                            {cons, 1,
                                {bin, 1, [
                                    {bin_element, 1, {string, 1, "</div>"}, default, default}
                                ]},
                                {cons, 1,
                                    {call, 1,
                                        {remote, 1, {atom, 1, eel_utils}, {atom, 1, to_binary}}, [
                                            {block, 1, [
                                                {'case', 1,
                                                    {op, 1, '>',
                                                        {call, 1,
                                                            {remote, 1, {atom, 1, erlang},
                                                                {atom, 1, length}},
                                                            [{var, 1, 'List'}]},
                                                        {integer, 1, 0}},
                                                    [
                                                        {clause, 1, [{atom, 1, true}], [], [
                                                            {call, 1,
                                                                {remote, 1, {atom, 1, erlang},
                                                                    {atom, 1, iolist_to_binary}},
                                                                [
                                                                    {cons, 1,
                                                                        {bin, 1, [
                                                                            {bin_element, 1,
                                                                                {string, 1, "<ul>"},
                                                                                default, default}
                                                                        ]},
                                                                        {cons, 1,
                                                                            {call, 1,
                                                                                {remote, 1,
                                                                                    {atom, 1,
                                                                                        eel_utils},
                                                                                    {atom, 1,
                                                                                        to_binary}},
                                                                                [
                                                                                    {block, 1, [
                                                                                        {call, 1,
                                                                                            {remote,
                                                                                                1,
                                                                                                {atom,
                                                                                                    1,
                                                                                                    lists},
                                                                                                {atom,
                                                                                                    1,
                                                                                                    map}},
                                                                                            [
                                                                                                {'fun',
                                                                                                    1,
                                                                                                    {clauses,
                                                                                                        [
                                                                                                            {clause,
                                                                                                                1,
                                                                                                                [
                                                                                                                    {var,
                                                                                                                        1,
                                                                                                                        'N'}
                                                                                                                ],
                                                                                                                [],
                                                                                                                [
                                                                                                                    {call,
                                                                                                                        1,
                                                                                                                        {remote,
                                                                                                                            1,
                                                                                                                            {atom,
                                                                                                                                1,
                                                                                                                                erlang},
                                                                                                                            {atom,
                                                                                                                                1,
                                                                                                                                iolist_to_binary}},
                                                                                                                        [
                                                                                                                            {cons,
                                                                                                                                1,
                                                                                                                                {bin,
                                                                                                                                    1,
                                                                                                                                    [
                                                                                                                                        {bin_element,
                                                                                                                                            1,
                                                                                                                                            {string,
                                                                                                                                                1,
                                                                                                                                                "<li>"},
                                                                                                                                            default,
                                                                                                                                            default}
                                                                                                                                    ]},
                                                                                                                                {cons,
                                                                                                                                    1,
                                                                                                                                    {call,
                                                                                                                                        1,
                                                                                                                                        {remote,
                                                                                                                                            1,
                                                                                                                                            {atom,
                                                                                                                                                1,
                                                                                                                                                eel_utils},
                                                                                                                                            {atom,
                                                                                                                                                1,
                                                                                                                                                to_binary}},
                                                                                                                                        [
                                                                                                                                            {block,
                                                                                                                                                1,
                                                                                                                                                [
                                                                                                                                                    {var,
                                                                                                                                                        1,
                                                                                                                                                        'N'}
                                                                                                                                                ]}
                                                                                                                                        ]},
                                                                                                                                    {cons,
                                                                                                                                        1,
                                                                                                                                        {bin,
                                                                                                                                            1,
                                                                                                                                            [
                                                                                                                                                {bin_element,
                                                                                                                                                    1,
                                                                                                                                                    {string,
                                                                                                                                                        1,
                                                                                                                                                        "</li>"},
                                                                                                                                                    default,
                                                                                                                                                    default}
                                                                                                                                            ]},
                                                                                                                                        {nil,
                                                                                                                                            1}}}}
                                                                                                                        ]}
                                                                                                                ]}
                                                                                                        ]}},
                                                                                                {call,
                                                                                                    1,
                                                                                                    {remote,
                                                                                                        1,
                                                                                                        {atom,
                                                                                                            1,
                                                                                                            lists},
                                                                                                        {atom,
                                                                                                            1,
                                                                                                            seq}},
                                                                                                    [
                                                                                                        {integer,
                                                                                                            1,
                                                                                                            1},
                                                                                                        {call,
                                                                                                            1,
                                                                                                            {remote,
                                                                                                                1,
                                                                                                                {atom,
                                                                                                                    1,
                                                                                                                    erlang},
                                                                                                                {atom,
                                                                                                                    1,
                                                                                                                    length}},
                                                                                                            [
                                                                                                                {var,
                                                                                                                    1,
                                                                                                                    'List'}
                                                                                                            ]}
                                                                                                    ]}
                                                                                            ]}
                                                                                    ]}
                                                                                ]},
                                                                            {cons, 1,
                                                                                {bin, 1, [
                                                                                    {bin_element, 1,
                                                                                        {string, 1,
                                                                                            "</ul>"},
                                                                                        default,
                                                                                        default}
                                                                                ]},
                                                                                {nil, 1}}}}
                                                                ]}
                                                        ]},
                                                        {clause, 1, [{atom, 1, false}], [], [
                                                            {bin, 1, []}
                                                        ]}
                                                    ]}
                                            ]}
                                        ]},
                                    {nil, 1}}}}}
                ]}
            ],
            ['List', 'List', 'List']
        }
    ]},
    ?assertEqual(Expected, compile(Bin)).

render_test() ->
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
    {Static, AST} = compile(Bin),
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
    {Render, Memo, {_, _, Indexes}} = render(Static, AST, Bindings),
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
    {MemoRender, _, {_, _, MemoIndexes}} = render(Static, AST, Memo, NewBindings),
    ?assertEqual(ExpectedMemoRender, MemoRender),
    ?assertEqual(ExpectedMemoIndexes, MemoIndexes).

-endif.
