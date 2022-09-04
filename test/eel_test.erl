-module(eel_test).

-include_lib("eunit/include/eunit.hrl").

-define(RE_WS_TRIM, re:compile(<<"^\\s+|\\s+$">>)).

% -type dynamic() :: {
%     ExprRef    :: atom(),
%     Markers    :: {StartMarker :: binary(), EndMarker :: binary()},
%     Expr       :: binary(),
%     UniqueVars :: [atom()],
%     Parts      :: {Static :: [binary(), Dynamic :: [atom()]},
% }.

tokenize_test() ->
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
        {
            [
                <<"<html><div>">>,
                <<"</div>">>,
                <<"</html>">>
            ],
            [
                [
                    {expr,
                        {
                            {<<"=">>, <<".">>},
                            <<"Foo = 1, Bar = Foo">>,
                            ['Foo', 'Bar'],
                            {[<<>>, <<" = 1, ">>, <<" = ">>, <<>>], ['Foo', 'Bar', 'Foo']}
                        }}
                ],
                [
                    {start_expr,
                        {
                            {<<"=">>, <<" ">>},
                            <<"case #{foo := Foo} = Map of">>,
                            ['Map'],
                            {[<<"case #{foo := Foo} = ">>, <<" of">>], ['Map']}
                        }},
                    {mid_expr,
                        {
                            {<<" ">>, <<" ">>},
                            <<"bar ->">>,
                            [],
                            {[<<"bar ->">>], []}
                        }},
                    {nested_expr, {
                        {
                            [
                                <<"<p>">>,
                                <<"</p>">>
                            ],
                            [
                                [
                                    {expr,
                                        {
                                            {<<"=">>, <<".">>},
                                            <<"Bar">>,
                                            ['Bar'],
                                            {[<<>>, <<>>], ['Bar']}
                                        }}
                                ]
                            ]
                        },
                        <<"<p><%= Bar .%></p>">>
                    }},
                    {mid_expr,
                        {
                            {<<" ">>, <<" ">>},
                            <<";">>,
                            [],
                            {[<<";">>], []}
                        }},
                    {mid_expr,
                        {
                            {<<" ">>, <<" ">>},
                            <<"foobar -> Foobar;">>,
                            ['Foobar'],
                            {[<<"foobar -> ">>, <<";">>], ['Foobar']}
                        }},
                    {end_expr,
                        {
                            {<<" ">>, <<".">>},
                            <<"Foo -> Foo end">>,
                            ['Foo'],
                            {[<<>>, <<" -> ">>, <<" end">>], ['Foo', 'Foo']}
                        }}
                ]
            ]
        },
        <<"<html><div><%= Foo = 1, Bar = Foo .%></div><%= case #{foo := Foo} = Map of %><% bar -> %><p><%= Bar .%></p><% ; %><% foobar -> Foobar; %><% Foo -> Foo end .%></html>">>
    },
    ?assertEqual(Expected, tokenize(Bin)).

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
    case nested(Bin, <<>>) of
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

nested(<<"<% ", _/binary>> = T, Expr) ->
    {ok, {Expr, T}};
nested(<<H, T/binary>>, Expr) ->
    nested(T, <<Expr/binary, H>>);
nested(<<>>, _Expr) ->
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

flatten_test() ->
    Dynamic = [
        [
            {expr,
                {
                    {<<"=">>, <<".">>},
                    <<"Foo = 1, Bar = Foo">>,
                    ['Foo', 'Bar'],
                    {[<<>>, <<" = 1, ">>, <<" = ">>, <<>>], ['Foo', 'Bar', 'Foo']}
                }}
        ],
        [
            {start_expr,
                {
                    {<<"=">>, <<" ">>},
                    <<"case #{foo := Foo} = Map of">>,
                    ['Map'],
                    {[<<"case #{foo := Foo} = ">>, <<" of">>], ['Map']}
                }},
            {mid_expr,
                {
                    {<<" ">>, <<" ">>},
                    <<"bar ->">>,
                    [],
                    {[<<"bar ->">>], []}
                }},
            {nested_expr, {
                {
                    [
                        <<"<p>">>,
                        <<>>,
                        <<"</p>">>
                    ],
                    [
                        [
                            {expr,
                                {
                                    {<<"=">>, <<".">>},
                                    <<"Bar">>,
                                    ['Bar'],
                                    {[<<>>, <<>>], ['Bar']}
                                }}
                        ]
                    ]
                },
                <<"<p><%= Bar .%></p>">>
            }},
            {mid_expr,
                {
                    {<<" ">>, <<" ">>},
                    <<";">>,
                    [],
                    {[<<";">>], []}
                }},
            {mid_expr,
                {
                    {<<" ">>, <<" ">>},
                    <<"foobar -> Foobar;">>,
                    ['Foobar'],
                    {[<<"foobar -> ">>, <<";">>], ['Foobar']}
                }},
            {end_expr,
                {
                    {<<" ">>, <<".">>},
                    <<"Foo -> Foo end">>,
                    ['Foo'],
                    {[<<>>, <<" -> ">>, <<" end">>], ['Foo', 'Foo']}
                }}
        ]
    ],
    Expected = [
        {
            <<"Foo = 1, Bar = Foo.">>,
            ['Foo', 'Bar']
        },
        {
            <<
                "case #{foo := Foo} = Map of "
                "bar -> erlang:iolist_to_binary([<<\"<p>\">>, Bar, <<>>, <<\"</p>\">>]); "
                "foobar -> Foobar; "
                "Foo -> Foo "
                "end."
            >>,
            ['Map', 'Bar', 'Foobar', 'Foo']
        }
    ],
    ?assertEqual(Expected, flatten(Dynamic)).

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
                    DynamicExpr = extract_dynamic_expression(Dynamic),
                    {IOList, Siblings} = join(Static, DynamicExpr),
                    IOListToBinFun = iolist_to_binary_fun(IOList, Siblings),
                    <<Acc/binary, IOListToBinFun/binary>>
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
extract_token_expression([], Acc) ->
    Acc.

join(Static, Dynamic) ->
    do_join(Static, Dynamic, [], []).

do_join([S | Static], [D0 | Dynamic], IOList, Siblings) ->
    % TODO: Dynamic expression must be wrapped with "to_binary_fun".
    %       Same as iolist_to_binary_fun/2 case,
    D = erlang:iolist_to_binary(D0),
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

parse_test() ->
    Flattened = [
        {
            <<"Foo = 1, Bar = Foo.">>,
            ['Foo', 'Bar']
        },
        {
            <<
                "case #{foo := Foo} = Map of "
                "bar -> erlang:iolist_to_binary([<<\"<p>\">>, Bar, <<>>, <<\"</p>\">>]); "
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
                                    {cons, 1, {var, 1, 'Bar'},
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
                                            {cons, 1, {var, 1, 'Bar'},
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

compile(Bin) ->
    % TODO: Remove Expression from tokenize return.
    %       It should be {Static, Dynamic}.
    {{Static, Dynamic0}, _Expression} = tokenize(Bin),
    Dynamic1 = flatten(Dynamic0),
    Dynamic = parse(Dynamic1),
    {Static, Dynamic}.

render_test() ->
    Bin = <<
        "<h1><%= Title .%></h1>"
        "<ul>"
        "<%= lists:map(fun(Item) -> %>"
        "<li><%= Item .%></li>"
        "<% end, List) .%>"
        "</ul>"
        "<div>Item count: <%= erlang:length(List) .%></div>"
    >>,
    {Static, Dynamic} = compile(Bin),
    Bindings = #{
        'Title' => <<"EEL">>,
        'List' => [<<"Foo">>, <<"Bar">>]
    },
    ExpectedRender = <<"<h1>EEL</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div>">>,
    ExpectedIndexes = #{
        1 => <<"EEL">>,
        2 => <<"<li>Foo</li><li>Bar</li>">>,
        3 => <<"2">>
    },
    {Render, Memo, {_, _, Indexes}} = render(Static, Dynamic, Bindings),
    ?assertEqual(ExpectedRender, Render),
    ?assertEqual(ExpectedIndexes, Indexes),

    NewBindings =
        Bindings = #{
            'Title' => <<"Embedded Erlang">>
        },
    ExpectedMemoRender =
        <<"<h1>Embedded Erlang</h1><ul><li>Foo</li><li>Bar</li></ul><div>Item count: 2</div>">>,
    ExpectedMemoIndexes = #{
        1 => <<"Embedded Erlang">>
    },
    {MemoRender, _, {_, _, MemoIndexes}} = render(Static, Dynamic, Memo, NewBindings),
    ?assertEqual(ExpectedMemoRender, MemoRender),
    ?assertEqual(ExpectedMemoIndexes, MemoIndexes).

render(Static, Compiled, Bindings) ->
    render(Static, Compiled, #{}, Bindings).

render(Static, Compiled, Memo, NewBindings) ->
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
                            Result = to_binary(Result0),
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
            Compiled
        ),
    Eval = lists:reverse(ReversedEval),
    Render = merge(Static, Eval),
    NewMemo = #{
        eval => Eval,
        bindings => Bindings
    },
    {Render, NewMemo, {Static, BindingsIndexes, NewBindingsIndexes}}.

to_binary(Value) ->
    to_binary(Value, undefined).

to_binary(Bin, _) when is_binary(Bin) ->
    Bin;
to_binary(undefined, _) ->
    <<>>;
to_binary(Atom, undefined) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
to_binary(Atom, Encoding) when is_atom(Atom), is_atom(Encoding) ->
    erlang:atom_to_binary(Atom, Encoding);
to_binary(Float, undefined) when is_float(Float) ->
    erlang:float_to_binary(Float);
to_binary(Float, Options) when is_float(Float), is_list(Options) ->
    erlang:float_to_binary(Float, Options);
to_binary(Int, undefined) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_binary(Int, Base) when is_integer(Int), is_integer(Base) ->
    erlang:integer_to_binary(Int, Base);
to_binary(List, undefined) when is_list(List) ->
    erlang:iolist_to_binary(List);
to_binary(Tuple, undefined) when is_tuple(Tuple) ->
    to_binary(erlang:tuple_to_list(Tuple));
to_binary(_, _) ->
    <<>>.
