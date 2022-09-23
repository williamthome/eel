%%%---------------------------------------------------------------------------------------
%%% @doc EEl compiler module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%---------------------------------------------------------------------------------------
-module(eel_compile).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    binary/1,
    file/1,
    priv_file/2
]).

-export_type([
    static/0,
    ast/0,
    return/0
]).

-type static() :: [binary()].
-type ast() :: [{[erl_parse:abstract_expr()], [atom()]}].
-type return() :: {static(), ast()}.

-define(RE_WS_TRIM, re:compile(<<"^\\s+|\\s+$">>)).

%%------------------------------------------------------------------------------
%% @doc Compiles a binary.
%% @end
%%------------------------------------------------------------------------------
-spec binary(binary()) -> return().

binary(Bin) ->
    {Static, Dynamic} = tokenize(Bin),
    Flattened = flatten(Dynamic),
    AST = parse(Flattened),
    {Static, AST}.

%%------------------------------------------------------------------------------
%% @doc Compiles a file joining the priv_dir to the file name.
%% @end
%%------------------------------------------------------------------------------
-spec priv_file(atom(), file:filename_all()) -> return() | {error, term()}.

priv_file(App, FileName0) ->
    PrivDir = code:priv_dir(App),
    FileName = filename:join([PrivDir, FileName0]),
    file(FileName).

%%------------------------------------------------------------------------------
%% @doc Compiles a file.
%% @end
%%------------------------------------------------------------------------------
-spec file(file:filename_all()) -> return() | {error, term()}.

file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> binary(Bin);
        {error, Reason} -> {error, Reason}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

tokenize(Bin) ->
    do_tokenize(Bin, {[], []}, []).

do_tokenize(<<"<%", _/binary>> = Bin, {Static0, Dynamic0}, TokensAcc) ->
    case tokenize_expr(Bin) of
        {ok, {ExprRef, Token, Rest}} ->
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
            do_tokenize(Rest, {Static, Dynamic}, [Token | TokensAcc]);
        {ok, {_Comment, Rest}} ->
            % TODO: Maybe the comment supress can be optional
            % Token = {comment, Comment},
            % Dynamic = [Token | Dynamic0],
            do_tokenize(Rest, {Static0, Dynamic0}, TokensAcc);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(Bin, {Static, [HD | Dynamic]}, [{ExprRef, _Expr} | _] = TokensAcc) when
    ExprRef =:= start_expr; ExprRef =:= mid_expr
->
    case nested(Bin, 0, <<>>) of
        {ok, {Nested, T}} ->
            Token = {nested_expr, tokenize(Nested)},
            do_tokenize(
                T,
                {Static, [[Token | HD] | Dynamic]},
                [Token | TokensAcc]
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<H, T/binary>>, {Static0, Dynamic}, TokensAcc0) ->
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
    do_tokenize(T, {Static, Dynamic}, TokensAcc);
do_tokenize(<<>>, {Static, Dynamic}, _TokensAcc) ->
    {lists:reverse(Static), lists:reverse(Dynamic)}.

tokenize_expr(<<"<%", T0/binary>>) ->
    {StartMarker, T} = retrieve_marker(T0, <<>>),
    tokenize_by_marker(StartMarker, T).

tokenize_by_marker(<<"%">>, Bin) ->
    comment(Bin, <<>>);
tokenize_by_marker(StartMarker, Bin) ->
    case expression(Bin, StartMarker, expr, <<>>) of
        {ok, {ExprRef, Expr, EndMarker, Rest}} ->
            Token = {ExprRef, {{StartMarker, EndMarker}, Expr}},
            {ok, {ExprRef, Token, Rest}};
        {error, Reason} ->
            {error, Reason}
    end.

comment(<<32, "%%>", T/binary>>, Cache) ->
    Comment = trim(Cache),
    {ok, {Comment, T}};
comment(<<"%%>", _/binary>>, _Cache) ->
    % TODO: Handle missing_space
    {error, missing_space};
comment(<<H, T/binary>>, Cache) ->
    comment(T, <<Cache/binary, H>>);
comment(<<>>, _Cache) ->
    {error, eof}.

expression(<<32, "%>", T/binary>>, StartMarker, expr, Cache) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> mid_expr;
            _ -> start_expr
        end,
    {ok, {ExprRef, Expr, <<32>>, T}};
expression(<<32, EndMarker, "%>", T/binary>>, StartMarker, expr, Cache) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> end_expr;
            _ -> expr
        end,
    {ok, {ExprRef, Expr, <<EndMarker>>, T}};
expression(<<"%>", T/binary>>, _StartMarker, expr, Cache) ->
    case is_cache_empty(Cache) of
        true ->
            {ok, {end_expr, <<>>, <<".">>, T}};
        false ->
            % TODO: Handle no end marker
            {error, no_end_marker}
    end;
expression(<<"<%", T/binary>>, StartMarker, expr, Cache) ->
    expression(T, StartMarker, text, <<Cache/binary, "<%">>);
expression(<<"<%", T/binary>>, StartMarker, text, Cache) ->
    expression(T, StartMarker, text, <<Cache/binary, "<%">>);
expression(<<"%>", T/binary>>, StartMarker, text, Cache) ->
    expression(T, StartMarker, expr, <<Cache/binary, "%>">>);
% expression(<<"eel_compile:binary(", T/binary>>, StartMarker, In, Cache0) ->
%     {ok, {Rest, Cache}} = ignore_fun_expr(T, 1, <<Cache0/binary, "eel_compile:binary(">>),
%     expression(Rest, StartMarker, In, Cache);
expression(<<H, T/binary>>, StartMarker, In, Cache) ->
    expression(T, StartMarker, In, <<Cache/binary, H>>);
expression(<<>>, _StartMarker, _In, _Cache) ->
    {error, eof}.

is_cache_empty(<<".">>) ->
    true;
is_cache_empty(<<32, T/binary>>) ->
    is_cache_empty(T);
is_cache_empty(<<_, _/binary>>) ->
    false;
is_cache_empty(<<>>) ->
    true.

% ignore_fun_expr(Bin, 0, Cache) ->
%     {ok, {Bin, Cache}};
% ignore_fun_expr(<<"(", T/binary>>, Count, Cache) ->
%     ignore_fun_expr(T, Count + 1, <<Cache/binary, "(">>);
% ignore_fun_expr(<<")", T/binary>>, Count, Cache) ->
%     ignore_fun_expr(T, Count - 1, <<Cache/binary, ")">>);
% ignore_fun_expr(<<H, T/binary>>, Count, Cache) ->
%     ignore_fun_expr(T, Count, <<Cache/binary, H>>).

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
    {<<32>>, T};
retrieve_marker(<<32, _/binary>> = T, Marker) ->
    {Marker, T};
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
        "eel_convert:to_binary(begin ", Acc, " end)"
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

parse(Flattened) ->
    lists:map(
        fun(Expr) ->
            Vars = retrieve_vars(Expr),
            {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Expr)),
            {ok, Exprs} = erl_parse:parse_exprs(Tokens),

            case compile_exprs(Exprs) of
                {true, _} ->
                    {Exprs, Vars};
                false ->
                    {Exprs, Vars}
            end
        end,
        Flattened
    ).

compile_exprs([{call, _, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, _}] = Exprs) ->
    CompileModFuns = [
        {eel, compile_binary},
        {eel, compile_file},
        {eel, compile_priv_file},
        {eel_compile, binary},
        {eel_compile, file},
        {eel_compile, priv_file}
    ],
    case lists:member({Mod, Fun}, CompileModFuns) of
        true ->
            {value, {Static, AST}, []} = erl_eval:exprs(Exprs, []),
            {true, {Static, AST}};
        false ->
            false
    end;
compile_exprs(_Exprs) ->
    false.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<
        "<h1><%= Title .%></h1>"
        "<h2><%= Bin = <<\"<b><%= Foo .%> | <%= Bar .%></b>\">>, eel_compile:binary(Bin) .%></h2>"
        "<ul>"
        "<%= lists:map(fun(Item) -> %>"
        "<li><%= Item .%></li>"
        "<% end, List) .%>"
        "</ul>"
        "<%= Length = erlang:length(List), %>"
        "<div>Item count: <%= Length .%></div>"
        "<%= case Length > 0 of true -> %>"
        "<ul>"
        "<%= lists:map(fun(N) -> %>"
        "<li><%= N .%></li>"
        "<% end, lists:seq(1, Length)) .%>"
        "</ul>"
        "<% ; false -> <<>> end .%>"
        "<% .%>"
    >>,
    Expected = {[<<"<h1>">>, <<"</h1><h2>">>, <<"</h2><ul>">>, <<"</ul>">>], [
        [{expr, {{<<"=">>, <<".">>}, <<"Title">>}}],
        [
            {expr, {
                {<<"=">>, <<".">>},
                <<"Bin = <<\"<b><%= Foo .%> | <%= Bar .%></b>\">>, eel_compile:binary(Bin)">>
            }}
        ],
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
                        {start_expr, {{<<"=">>, <<" ">>}, <<"case Length > 0 of true ->">>}},
                        {nested_expr,
                            {[<<"<ul>">>, <<"</ul>">>], [
                                [
                                    {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(N) ->">>}},
                                    {nested_expr,
                                        {[<<"<li>">>, <<"</li>">>], [
                                            [{expr, {{<<"=">>, <<".">>}, <<"N">>}}]
                                        ]}},
                                    {end_expr, {
                                        {<<" ">>, <<".">>}, <<"end, lists:seq(1, Length))">>
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
            {expr, {
                {<<"=">>, <<".">>},
                <<"Bin = <<\"<b><%= Foo .%> | <%= Bar .%></b>\">>, eel_compile:binary(Bin)">>
            }}
        ],
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
                        {start_expr, {{<<"=">>, <<" ">>}, <<"case Length > 0 of true ->">>}},
                        {nested_expr,
                            {[<<"<ul>">>, <<"</ul>">>], [
                                [
                                    {start_expr, {{<<"=">>, <<" ">>}, <<"lists:map(fun(N) ->">>}},
                                    {nested_expr,
                                        {[<<"<li>">>, <<"</li>">>], [
                                            [{expr, {{<<"=">>, <<".">>}, <<"N">>}}]
                                        ]}},
                                    {end_expr, {
                                        {<<" ">>, <<".">>}, <<"end, lists:seq(1, Length))">>
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
        <<"Bin = <<\"<b><%= Foo .%> | <%= Bar .%></b>\">>, eel_compile:binary(Bin).">>,
        <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>,
        <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_convert:to_binary(begin Length end), <<\"</div>\">>, eel_convert:to_binary(begin case Length > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_convert:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, Length)) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
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
                <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>
            )
        ),
        ?assertEqual(
            ['List', 'List', 'List'],
            retrieve_vars(
                <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_convert:to_binary(begin Length end), <<\"</div>\">>, eel_convert:to_binary(begin case erlang:length(List) > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_convert:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, erlang:length(List))) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
            )
        )
    ].

parse_test() ->
    Flattened = [
        <<"Title.">>,
        <<"Bin = <<\"<b><%= Foo .%> | <%= Bar .%></b>\">>, eel_compile:binary(Bin).">>,
        <<"lists:map(fun(Item) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin Item end), <<\"</li>\">>]) end, List).">>,
        <<"Length = erlang:length(List), erlang:iolist_to_binary([<<\"<div>Item count: \">>, eel_convert:to_binary(begin Length end), <<\"</div>\">>, eel_convert:to_binary(begin case Length > 0 of true -> erlang:iolist_to_binary([<<\"<ul>\">>, eel_convert:to_binary(begin lists:map(fun(N) -> erlang:iolist_to_binary([<<\"<li>\">>, eel_convert:to_binary(begin N end), <<\"</li>\">>]) end, lists:seq(1, Length)) end), <<\"</ul>\">>]) ; false -> <<>> end end)]) .">>
    ],
    Expected = [
        {[{var, 1, 'Title'}], ['Title']},
        {
            [
                {match, 1, {var, 1, 'Bin'},
                    {bin, 1, [
                        {bin_element, 1, {string, 1, "<b><%= Foo .%> | <%= Bar .%></b>"}, default,
                            default}
                    ]}},
                {call, 1, {remote, 1, {atom, 1, eel_compile}, {atom, 1, binary}}, [{var, 1, 'Bin'}]}
            ],
            []
        },
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
                                                    {remote, 1, {atom, 1, eel_convert},
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
                            {call, 1, {remote, 1, {atom, 1, eel_convert}, {atom, 1, to_binary}}, [
                                {block, 1, [{var, 1, 'Length'}]}
                            ]},
                            {cons, 1,
                                {bin, 1, [
                                    {bin_element, 1, {string, 1, "</div>"}, default, default}
                                ]},
                                {cons, 1,
                                    {call, 1,
                                        {remote, 1, {atom, 1, eel_convert}, {atom, 1, to_binary}}, [
                                            {block, 1, [
                                                {'case', 1,
                                                    {op, 1, '>', {var, 1, 'Length'},
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
                                                                                        eel_convert},
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
                                                                                                                                                eel_convert},
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
                                                                                                        {var,
                                                                                                            1,
                                                                                                            'Length'}
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
            ['List']
        }
    ],
    ?assertEqual(Expected, parse(Flattened)).

% compile_test() ->
%     Bin =
%         <<
%             "<h1><%= Title .%></h1>"
%             "<ul>"
%             "<%= lists:map(fun(Item) -> %>"
%             "<li><%= Item .%></li>"
%             "<% end, List) .%>"
%             "</ul>"
%             "<%= Length = erlang:length(List), %>"
%             "<div>Item count: <%= Length .%></div>"
%             "<%= case erlang:length(List) > 0 of true -> %>"
%             "<ul>"
%             "<%= lists:map(fun(N) -> %>"
%             "<li><%= N .%></li>"
%             "<% end, lists:seq(1, erlang:length(List))) .%>"
%             "</ul>"
%             "<% ; false -> <<>> end .%>"
%             "<% .%>"
%         >>,
%     Expected = {[<<"<h1>">>, <<"</h1><ul>">>, <<"</ul>">>], [
%         {[{var, 1, 'Title'}], ['Title']},
%         {
%             [
%                 {call, 1, {remote, 1, {atom, 1, lists}, {atom, 1, map}}, [
%                     {'fun', 1,
%                         {clauses, [
%                             {clause, 1, [{var, 1, 'Item'}], [], [
%                                 {call, 1,
%                                     {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
%                                         {cons, 1,
%                                             {bin, 1, [
%                                                 {bin_element, 1, {string, 1, "<li>"}, default,
%                                                     default}
%                                             ]},
%                                             {cons, 1,
%                                                 {call, 1,
%                                                     {remote, 1, {atom, 1, eel_convert},
%                                                         {atom, 1, to_binary}},
%                                                     [{block, 1, [{var, 1, 'Item'}]}]},
%                                                 {cons, 1,
%                                                     {bin, 1, [
%                                                         {bin_element, 1, {string, 1, "</li>"},
%                                                             default, default}
%                                                     ]},
%                                                     {nil, 1}}}}
%                                     ]}
%                             ]}
%                         ]}},
%                     {var, 1, 'List'}
%                 ]}
%             ],
%             ['List']
%         },
%         {
%             [
%                 {match, 1, {var, 1, 'Length'},
%                     {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, length}}, [
%                         {var, 1, 'List'}
%                     ]}},
%                 {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, iolist_to_binary}}, [
%                     {cons, 1,
%                         {bin, 1, [
%                             {bin_element, 1, {string, 1, "<div>Item count: "}, default, default}
%                         ]},
%                         {cons, 1,
%                             {call, 1, {remote, 1, {atom, 1, eel_convert}, {atom, 1, to_binary}}, [
%                                 {block, 1, [{var, 1, 'Length'}]}
%                             ]},
%                             {cons, 1,
%                                 {bin, 1, [
%                                     {bin_element, 1, {string, 1, "</div>"}, default, default}
%                                 ]},
%                                 {cons, 1,
%                                     {call, 1,
%                                         {remote, 1, {atom, 1, eel_convert}, {atom, 1, to_binary}}, [
%                                             {block, 1, [
%                                                 {'case', 1,
%                                                     {op, 1, '>',
%                                                         {call, 1,
%                                                             {remote, 1, {atom, 1, erlang},
%                                                                 {atom, 1, length}},
%                                                             [{var, 1, 'List'}]},
%                                                         {integer, 1, 0}},
%                                                     [
%                                                         {clause, 1, [{atom, 1, true}], [], [
%                                                             {call, 1,
%                                                                 {remote, 1, {atom, 1, erlang},
%                                                                     {atom, 1, iolist_to_binary}},
%                                                                 [
%                                                                     {cons, 1,
%                                                                         {bin, 1, [
%                                                                             {bin_element, 1,
%                                                                                 {string, 1, "<ul>"},
%                                                                                 default, default}
%                                                                         ]},
%                                                                         {cons, 1,
%                                                                             {call, 1,
%                                                                                 {remote, 1,
%                                                                                     {atom, 1,
%                                                                                         eel_convert},
%                                                                                     {atom, 1,
%                                                                                         to_binary}},
%                                                                                 [
%                                                                                     {block, 1, [
%                                                                                         {call, 1,
%                                                                                             {remote,
%                                                                                                 1,
%                                                                                                 {atom,
%                                                                                                     1,
%                                                                                                     lists},
%                                                                                                 {atom,
%                                                                                                     1,
%                                                                                                     map}},
%                                                                                             [
%                                                                                                 {'fun',
%                                                                                                     1,
%                                                                                                     {clauses,
%                                                                                                         [
%                                                                                                             {clause,
%                                                                                                                 1,
%                                                                                                                 [
%                                                                                                                     {var,
%                                                                                                                         1,
%                                                                                                                         'N'}
%                                                                                                                 ],
%                                                                                                                 [],
%                                                                                                                 [
%                                                                                                                     {call,
%                                                                                                                         1,
%                                                                                                                         {remote,
%                                                                                                                             1,
%                                                                                                                             {atom,
%                                                                                                                                 1,
%                                                                                                                                 erlang},
%                                                                                                                             {atom,
%                                                                                                                                 1,
%                                                                                                                                 iolist_to_binary}},
%                                                                                                                         [
%                                                                                                                             {cons,
%                                                                                                                                 1,
%                                                                                                                                 {bin,
%                                                                                                                                     1,
%                                                                                                                                     [
%                                                                                                                                         {bin_element,
%                                                                                                                                             1,
%                                                                                                                                             {string,
%                                                                                                                                                 1,
%                                                                                                                                                 "<li>"},
%                                                                                                                                             default,
%                                                                                                                                             default}
%                                                                                                                                     ]},
%                                                                                                                                 {cons,
%                                                                                                                                     1,
%                                                                                                                                     {call,
%                                                                                                                                         1,
%                                                                                                                                         {remote,
%                                                                                                                                             1,
%                                                                                                                                             {atom,
%                                                                                                                                                 1,
%                                                                                                                                                 eel_convert},
%                                                                                                                                             {atom,
%                                                                                                                                                 1,
%                                                                                                                                                 to_binary}},
%                                                                                                                                         [
%                                                                                                                                             {block,
%                                                                                                                                                 1,
%                                                                                                                                                 [
%                                                                                                                                                     {var,
%                                                                                                                                                         1,
%                                                                                                                                                         'N'}
%                                                                                                                                                 ]}
%                                                                                                                                         ]},
%                                                                                                                                     {cons,
%                                                                                                                                         1,
%                                                                                                                                         {bin,
%                                                                                                                                             1,
%                                                                                                                                             [
%                                                                                                                                                 {bin_element,
%                                                                                                                                                     1,
%                                                                                                                                                     {string,
%                                                                                                                                                         1,
%                                                                                                                                                         "</li>"},
%                                                                                                                                                     default,
%                                                                                                                                                     default}
%                                                                                                                                             ]},
%                                                                                                                                         {nil,
%                                                                                                                                             1}}}}
%                                                                                                                         ]}
%                                                                                                                 ]}
%                                                                                                         ]}},
%                                                                                                 {call,
%                                                                                                     1,
%                                                                                                     {remote,
%                                                                                                         1,
%                                                                                                         {atom,
%                                                                                                             1,
%                                                                                                             lists},
%                                                                                                         {atom,
%                                                                                                             1,
%                                                                                                             seq}},
%                                                                                                     [
%                                                                                                         {integer,
%                                                                                                             1,
%                                                                                                             1},
%                                                                                                         {call,
%                                                                                                             1,
%                                                                                                             {remote,
%                                                                                                                 1,
%                                                                                                                 {atom,
%                                                                                                                     1,
%                                                                                                                     erlang},
%                                                                                                                 {atom,
%                                                                                                                     1,
%                                                                                                                     length}},
%                                                                                                             [
%                                                                                                                 {var,
%                                                                                                                     1,
%                                                                                                                     'List'}
%                                                                                                             ]}
%                                                                                                     ]}
%                                                                                             ]}
%                                                                                     ]}
%                                                                                 ]},
%                                                                             {cons, 1,
%                                                                                 {bin, 1, [
%                                                                                     {bin_element, 1,
%                                                                                         {string, 1,
%                                                                                             "</ul>"},
%                                                                                         default,
%                                                                                         default}
%                                                                                 ]},
%                                                                                 {nil, 1}}}}
%                                                                 ]}
%                                                         ]},
%                                                         {clause, 1, [{atom, 1, false}], [], [
%                                                             {bin, 1, []}
%                                                         ]}
%                                                     ]}
%                                             ]}
%                                         ]},
%                                     {nil, 1}}}}}
%                 ]}
%             ],
%             ['List', 'List', 'List']
%         }
%     ]},
%     ?assertEqual(Expected, binary(Bin)).

the_test() ->
    {Static, Dynamic} = tokenize(
        <<"eel_compile:binary(<<\"<b><%= Foo .%> | <%= Bar .%></b>\">>).">>
    ),
    Flattened = flatten(Dynamic),
    ?debugFmt("~p", [{Static, Flattened}]).

-endif.
