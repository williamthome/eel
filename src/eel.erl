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
    % TODO: Remove Expression from tokenize return.
    %       It should be {Static, Dynamic}.
    {{Static, Dynamic}, _Expression} = tokenize(Bin),
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
do_tokenize(<<>>, {Static, Dynamic}, _TokensAcc, Acc) ->
    {{lists:reverse(Static), lists:reverse(Dynamic)}, Acc}.

tokenize_expr(<<"<%", T0/binary>>, Acc0) ->
    {StartMarker, MaybeSpace, T} = retrieve_marker(T0, <<>>),
    Acc1 = <<Acc0/binary, "<%", StartMarker/binary, MaybeSpace/binary>>,
    tokenize_by_marker(StartMarker, T, Acc1).

tokenize_by_marker(<<"#">>, Bin, Acc0) ->
    comment(Bin, <<>>, Acc0);
tokenize_by_marker(StartMarker, Bin, Acc1) ->
    case expression(Bin, StartMarker, <<>>, Acc1) of
        {ok, {ExprRef, Expr, EndMarker, Rest, Acc}} ->
            Vars = retrieve_vars(Expr),
            Parts = split_expr(Expr, Vars),
            UniqueVars = unique(Vars),
            Token = {ExprRef, {{StartMarker, EndMarker}, Expr, UniqueVars, Parts}},
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
expression(<<"%>", _/binary>>, _StartMarker, _Cache, _Acc) ->
    % TODO: Handle unknown end marker
    {error, unknown_end_marker};
expression(<<H, T/binary>>, StartMarker, Cache, Acc) ->
    expression(T, StartMarker, <<Cache/binary, H>>, Acc);
expression(<<>>, _StartMarker, _Cache, _Acc) ->
    {error, eof}.

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
    do_retrieve_vars(Tokens, []).

do_retrieve_vars([{':=', _}, {var, _, _} | Tokens], Acc) ->
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([{var, _, Var} | Tokens], Acc0) ->
    Acc = [Var | Acc0],
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([_Token | Tokens], Acc) ->
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([], Acc) ->
    lists:reverse(Acc).

split_expr(Expr, []) ->
    {[Expr], []};
split_expr(Expr, Vars) ->
    VarsBin = lists:map(fun erlang:atom_to_binary/1, Vars),
    Static = binary:split(Expr, VarsBin, [global]),
    {Static, Vars}.

unique([]) -> [];
unique([H | T]) -> [H | [X || X <- unique(T), X =/= H]].

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
    Acc.

flatten(Dynamic) ->
    lists:map(fun merge_expression/1, Dynamic).

merge_expression(Expression) ->
    Merged =
        lists:foldl(
            fun
                ({ERef, {_, _, _, {Static, Dynamic}}}, Acc) ->
                    Merged = merge(Static, Dynamic),
                    MaybeSpace =
                        case lists:member(ERef, [expr, end_expr]) of
                            true -> <<>>;
                            false -> <<32>>
                        end,
                    <<Acc/binary, Merged/binary, MaybeSpace/binary>>;
                ({nested_expr, {{Static, Dynamic}, _}}, Acc) ->
                    NestedExpression = nested_expression(Static, Dynamic),
                    <<Acc/binary, NestedExpression/binary, 32>>
            end,
            <<>>,
            Expression
        ),
    ExpressionEnddingWithDot = <<Merged/binary, $.>>,
    ExpressionVars = extract_expression_vars(Expression),
    {ExpressionEnddingWithDot, ExpressionVars}.

extract_dynamic_expression(Dynamic) ->
    do_extract_dynamic_expression(Dynamic, []).

do_extract_dynamic_expression([Token | Dynamic], Acc) ->
    Expression = extract_token_expression(Token, Acc),
    do_extract_dynamic_expression(Dynamic, [Expression | Acc]);
do_extract_dynamic_expression([], Acc) ->
    lists:reverse(Acc).

extract_token_expression([{_, {_, Expr, _, _}} | Tokens], Acc) ->
    extract_token_expression(Tokens, [Expr | Acc]);
extract_token_expression([{nested_expr, {{Static, Dynamic}, _}} | Tokens], Acc) ->
    NestedExpression = nested_expression(Static, Dynamic),
    extract_token_expression(Tokens, [NestedExpression | Acc]);
extract_token_expression([], Acc) ->
    erlang:iolist_to_binary([
        "eel_utils:to_binary(", lists:join(" ", lists:reverse(Acc)), ")"
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

extract_expression_vars(Expression) ->
    Vars =
        lists:foldl(
            fun
                ({_, {_, _, ExprVars, _}}, Acc) ->
                    [lists:reverse(ExprVars) | Acc];
                ({nested_expr, {{_, Dynamic}, _}}, Acc) ->
                    NAcc =
                        lists:foldl(
                            fun(E, A) ->
                                [extract_expression_vars(E) | A]
                            end,
                            [],
                            Dynamic
                        ),
                    [NAcc | Acc]
            end,
            [],
            Expression
        ),
    lists:reverse(lists:flatten(Vars)).

% TODO: Return ok or error tuple.
parse(Flattened) ->
    lists:map(
        fun({Expr, Vars}) ->
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
        "<div>Item count: <%= erlang:length(List) .%></div>"
        "<%= case erlang:length(List) > 0 of true -> %>"
        "<ul>"
        "<%= lists:map(fun(N) -> %>"
        "<li><%= N .%></li>"
        "<% end, lists:seq(1, erlang:length(List))) .%>"
        "</ul>"
        "<% ; false -> <<>> end .%>"
    >>,
    Expected = {
        {
            [
                <<"<h1>">>,
                <<"</h1><ul>">>,
                <<"</ul><div>Item count: ">>,
                <<"</div>">>
            ],
            [
                [
                    {expr, {{<<"=">>, <<".">>}, <<"Title">>, ['Title'], {[<<>>, <<>>], ['Title']}}}
                ],
                [
                    {start_expr,
                        {
                            {<<"=">>, <<" ">>},
                            <<"lists:map(fun(Item) ->">>,
                            ['Item'],
                            {[<<"lists:map(fun(">>, <<") ->">>], ['Item']}
                        }},
                    {nested_expr, {
                        {[<<"<li>">>, <<"</li>">>], [
                            [
                                {expr,
                                    {
                                        {<<"=">>, <<".">>},
                                        <<"Item">>,
                                        ['Item'],
                                        {[<<>>, <<>>], ['Item']}
                                    }}
                            ]
                        ]},
                        <<"<li><%= Item .%></li>">>
                    }},
                    {end_expr,
                        {
                            {<<" ">>, <<".">>},
                            <<"end, List)">>,
                            ['List'],
                            {[<<"end, ">>, <<")">>], ['List']}
                        }}
                ],
                [
                    {expr,
                        {
                            {<<"=">>, <<".">>},
                            <<"erlang:length(List)">>,
                            ['List'],
                            {[<<"erlang:length(">>, <<")">>], ['List']}
                        }}
                ],
                [
                    {start_expr,
                        {
                            {<<"=">>, <<" ">>},
                            <<"case erlang:length(List) > 0 of true ->">>,
                            ['List'],
                            {[<<"case erlang:length(">>, <<") > 0 of true ->">>], ['List']}
                        }},
                    {nested_expr, {
                        {[<<"<ul>">>, <<"</ul>">>], [
                            [
                                {start_expr,
                                    {
                                        {<<"=">>, <<" ">>},
                                        <<"lists:map(fun(N) ->">>,
                                        ['N'],
                                        {[<<"lists:map(fun(">>, <<") ->">>], ['N']}
                                    }},
                                {nested_expr, {
                                    {[<<"<li>">>, <<"</li>">>], [
                                        [
                                            {expr,
                                                {
                                                    {<<"=">>, <<".">>},
                                                    <<"N">>,
                                                    ['N'],
                                                    {[<<>>, <<>>], ['N']}
                                                }}
                                        ]
                                    ]},
                                    <<"<li><%= N .%></li>">>
                                }},
                                {end_expr,
                                    {
                                        {<<" ">>, <<".">>},
                                        <<"end, lists:seq(1, erlang:length(List)))">>,
                                        ['List'],
                                        {
                                            [
                                                <<"end, lists:seq(1, erlang:length(">>,
                                                <<")))">>
                                            ],
                                            ['List']
                                        }
                                    }}
                            ]
                        ]},
                        <<"<ul><%= lists:map(fun(N) -> %><li><%= N .%></li><% end, lists:seq(1, erlang:length(List))) .%></ul>">>
                    }},
                    {end_expr,
                        {
                            {<<" ">>, <<".">>},
                            <<"; false -> <<>> end">>,
                            [],
                            {[<<"; false -> <<>> end">>], []}
                        }}
                ]
            ]
        },
        <<"<h1><%= Title .%></h1><ul><%= lists:map(fun(Item) -> %><li><%= Item .%></li><% end, List) .%></ul><div>Item count: <%= erlang:length(List) .%></div><%= case erlang:length(List) > 0 of true -> %><ul><%= lists:map(fun(N) -> %><li><%= N .%></li><% end, lists:seq(1, erlang:length(List))) .%></ul><% ; false -> <<>> end .%>">>
    },
    ?assertEqual(Expected, tokenize(Bin)).

flatten_test() ->
    Dynamic = [
        [
            {expr, {{<<"=">>, <<".">>}, <<"Title">>, ['Title'], {[<<>>, <<>>], ['Title']}}}
        ],
        [
            {start_expr,
                {
                    {<<"=">>, <<" ">>},
                    <<"lists:map(fun(Item) ->">>,
                    ['Item'],
                    {[<<"lists:map(fun(">>, <<") ->">>], ['Item']}
                }},
            {nested_expr, {
                {
                    [<<"<li>">>, <<"</li>">>],
                    [
                        [
                            {expr,
                                {
                                    {<<"=">>, <<".">>},
                                    <<"Item">>,
                                    ['Item'],
                                    {[<<>>, <<>>], ['Item']}
                                }}
                        ]
                    ]
                },
                <<"<li><%= Item .%></li>">>
            }},
            {end_expr,
                {
                    {<<" ">>, <<".">>},
                    <<"end, List)">>,
                    ['List'],
                    {[<<"end, ">>, <<")">>], ['List']}
                }}
        ],
        [
            {expr,
                {
                    {<<"=">>, <<".">>},
                    <<"erlang:length(List)">>,
                    ['List'],
                    {[<<"erlang:length(">>, <<")">>], ['List']}
                }}
        ],
        [
            {start_expr,
                {
                    {<<"=">>, <<" ">>},
                    <<"case erlang:length(List) > 0 of true ->">>,
                    ['List'],
                    {[<<"case erlang:length(">>, <<") > 0 of true ->">>], ['List']}
                }},
            {nested_expr, {
                {[<<"<ul>">>, <<"</ul>">>], [
                    [
                        {start_expr,
                            {
                                {<<"=">>, <<" ">>},
                                <<"lists:map(fun(N) ->">>,
                                ['N'],
                                {[<<"lists:map(fun(">>, <<") ->">>], ['N']}
                            }},
                        {nested_expr, {
                            {[<<"<li>">>, <<"</li>">>], [
                                [
                                    {expr,
                                        {
                                            {<<"=">>, <<".">>},
                                            <<"N">>,
                                            ['N'],
                                            {[<<>>, <<>>], ['N']}
                                        }}
                                ]
                            ]},
                            <<"<li><%= N .%></li>">>
                        }},
                        {end_expr,
                            {
                                {<<" ">>, <<".">>},
                                <<"end, lists:seq(1, erlang:length(List)))">>,
                                ['List'],
                                {
                                    [
                                        <<"end, lists:seq(1, erlang:length(">>,
                                        <<")))">>
                                    ],
                                    ['List']
                                }
                            }}
                    ]
                ]},
                <<"<ul><%= lists:map(fun(N) -> %><li><%= N .%></li><% end, lists:seq(1, erlang:length(List))) .%></ul>">>
            }},
            {end_expr,
                {
                    {<<" ">>, <<".">>},
                    <<"; false -> <<>> end">>,
                    [],
                    {[<<"; false -> <<>> end">>], []}
                }}
        ]
    ],
    Expected = [
        {
            <<"Title.">>,
            ['Title']
        },
        {
            <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(Item), <<\"</li>\">>]) end, List).">>,
            ['Item', 'Item', 'List']
        },
        {
            <<"erlang:length(List).">>,
            ['List']
        },
        {
            <<"case erlang:length(List) > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_utils:to_binary(lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_utils:to_binary(N), <<\"</li>\">>]) end, lists:seq(1, erlang:length(List)))), <<\"</ul>\">>]) ; false -> <<>> end.">>,
            ['List', 'List', 'N', 'N']
        }
    ],
    ?assertEqual(Expected, flatten(Dynamic)).

parse_test() ->
    Flattened = [
        {
            <<"Foo = 1, Bar = Foo.">>,
            ['Foo', 'Bar']
        },
        {
            <<
                "case #{foo := Foo} = Map of "
                "bar -> erlang:iolist_to_binary([<<\"<p>\">>, eel_utils:to_binary(Bar), <<>>, <<\"</p>\">>]); "
                "foobar -> Foobar; "
                "Foo -> Foo "
                "end."
            >>,
            ['Map', 'Bar', 'Foobar', 'Foo']
        }
    ],
    Expected = [
        {
            [
                {match, 1, {var, 1, 'Foo'}, {integer, 1, 1}},
                {match, 1, {var, 1, 'Bar'}, {var, 1, 'Foo'}}
            ],
            ['Foo', 'Bar']
        },
        {
            [
                {'case', 1,
                    {match, 1, {map, 1, [{map_field_exact, 1, {atom, 1, foo}, {var, 1, 'Foo'}}]},
                        {var, 1, 'Map'}},
                    [
                        {clause, 1, [{atom, 1, bar}], [], [
                            {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                                {cons, 1,
                                    {bin, 1, [
                                        {bin_element, 1, {string, 1, "<p>"}, default, default}
                                    ]},
                                    {cons, 1,
                                        {call, 1,
                                            {remote, 1, {atom, 1, eel_utils}, {atom, 1, to_binary}},
                                            [{var, 1, 'Bar'}]},
                                        {cons, 1, {bin, 1, []},
                                            {cons, 1,
                                                {bin, 1, [
                                                    {bin_element, 1, {string, 1, "</p>"}, default,
                                                        default}
                                                ]},
                                                {nil, 1}}}}}
                            ]}
                        ]},
                        {clause, 1, [{atom, 1, foobar}], [], [{var, 1, 'Foobar'}]},
                        {clause, 1, [{var, 1, 'Foo'}], [], [{var, 1, 'Foo'}]}
                    ]}
            ],
            ['Map', 'Bar', 'Foobar', 'Foo']
        }
    ],
    ?assertEqual(Expected, parse(Flattened)).

compile_test() ->
    Bin = <<
        "<%# Comment #%>"
        "<html>"
        "<div>"
        "<%=   Foo = 1, Bar = Foo   .%>"
        "</div>"
        "<%# Comment #%>"
        "<%=   case #{foo := Foo} = Map of %>"
        "<%   bar -> %>"
        "<p><%= Bar .%></p>"
        "<% ; %>"
        "<%   foobar -> Foobar; %>"
        "<%   Foo -> Foo end .%>"
        "</html>"
    >>,
    Expected = {
        [
            <<"<html><div>">>,
            <<"</div>">>,
            <<"</html>">>
        ],
        [
            {
                [
                    {match, 1, {var, 1, 'Foo'}, {integer, 1, 1}},
                    {match, 1, {var, 1, 'Bar'}, {var, 1, 'Foo'}}
                ],
                ['Foo', 'Bar']
            },
            {
                [
                    {'case', 1,
                        {match, 1,
                            {map, 1, [{map_field_exact, 1, {atom, 1, foo}, {var, 1, 'Foo'}}]},
                            {var, 1, 'Map'}},
                        [
                            {clause, 1, [{atom, 1, bar}], [], [
                                {call, 1,
                                    {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
                                        {cons, 1,
                                            {bin, 1, [
                                                {bin_element, 1, {string, 1, "<p>"}, default,
                                                    default}
                                            ]},
                                            {cons, 1,
                                                {call, 1,
                                                    {remote, 1, {atom, 1, eel_utils},
                                                        {atom, 1, to_binary}},
                                                    [{var, 1, 'Bar'}]},
                                                {cons, 1,
                                                    {bin, 1, [
                                                        {bin_element, 1, {string, 1, "</p>"},
                                                            default, default}
                                                    ]},
                                                    {nil, 1}}}}
                                    ]}
                            ]},
                            {clause, 1, [{atom, 1, foobar}], [], [{var, 1, 'Foobar'}]},
                            {clause, 1, [{var, 1, 'Foo'}], [], [{var, 1, 'Foo'}]}
                        ]}
                ],
                ['Map', 'Bar', 'Foobar', 'Foo']
            }
        ]
    },
    ?assertEqual(Expected, compile(Bin)).

render_test() ->
    Bin = <<
        "<h1><%= Title .%></h1>"
        "<ul>"
        "<%= lists:map(fun(Item) -> %>"
        "<li><%= Item .%></li>"
        "<% end, List) .%>"
        "</ul>"
        "<div>Item count: <%= erlang:length(List) .%></div>"
        "<%= case erlang:length(List) > 0 of true -> %>"
        "<ul>"
        "<%= lists:map(fun(N) -> %>"
        "<li><%= N .%></li>"
        "<% end, lists:seq(1, erlang:length(List))) .%>"
        "</ul>"
        "<% ; false -> <<>> end .%>"
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
        3 => <<"2">>,
        4 => <<"<ul><li>1</li><li>2</li></ul>">>
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
