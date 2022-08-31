%%%-----------------------------------------------------------------------------
%%% @doc Embedded Erlang library.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-dialyzer({nowarn_function, to_binary/2}).

-export([
    compile/1,
    compile_file/1,
    render/2,
    render_file/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec compile(binary()) -> {list(), list()}.

compile(Bin) ->
    resolve_parsed_tokens(parse_tokens(tokens(Bin))).

-spec compile_file(file:name_all()) -> {list(), list()}.

compile_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> compile(Bin);
        _ -> {[], []}
    end.

-spec render(binary() | {list(), list()}, map()) -> binary().

render({Static, Dynamic0}, Bindings) ->
    RenderFuns = map_dynamic_to_render_fun(Dynamic0, Bindings),
    Dynamic = lists:map(fun(Render) -> eval(Render, Bindings) end, RenderFuns),
    merge_bin_lists(Static, Dynamic);
render(Bin, Bindings) ->
    {Static, Dynamic} = compile(Bin),
    render({Static, Dynamic}, Bindings).

-spec render_file(file:name_all(), map()) -> binary().

render_file(FileName, Bindings) ->
    case file:read_file(FileName) of
        {ok, Bin} -> render(Bin, Bindings);
        _ -> <<>>
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

tokens(Bin) ->
    do_tokens(Bin, static, []).

do_tokens(<<"<%", Marker, Bin/binary>>, _, Acc) ->
    do_tokens(Bin, dynamic_start, [{dynamic, <<Marker>>, <<>>} | Acc]);
do_tokens(<<MarkerEnd, "%>", Bin/binary>>, dynamic_start, [{dynamic, MarkerStart, Expr} | Acc]) ->
    do_tokens(Bin, dynamic_end, [{dynamic, {MarkerStart, <<MarkerEnd>>}, Expr} | Acc]);
do_tokens(<<H, Bin/binary>>, dynamic_end, [{dynamic, Marker, Expr} | Acc]) ->
    do_tokens(Bin, static, [{static, <<H>>}, {dynamic, Marker, Expr} | Acc]);
do_tokens(<<H, Bin/binary>>, Kind, [{static, Text} | Acc]) ->
    do_tokens(Bin, Kind, [{static, <<Text/binary, H>>} | Acc]);
do_tokens(<<H, Bin/binary>>, Kind, [{Type, Marker, Expr} | Acc]) ->
    do_tokens(Bin, Kind, [{Type, Marker, <<Expr/binary, H>>} | Acc]);
do_tokens(<<H, Bin/binary>>, static, []) ->
    do_tokens(Bin, static, [{static, <<H>>}]);
do_tokens(<<>>, _, Acc) ->
    lists:reverse(Acc).

parse_tokens(Tokens) ->
    do_parse_tokens(Tokens, []).

do_parse_tokens([{static, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{text, Expr} | Acc]);
do_parse_tokens([{dynamic, {<<"=">>, <<".">>}, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{{expr, inline}, Expr} | Acc]);
do_parse_tokens([{dynamic, {<<"=">>, <<" ">>}, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{{expr, start}, <<Expr/binary, 32>>} | Acc]);
do_parse_tokens([{dynamic, {<<" ">>, <<" ">>}, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{{expr, continue}, <<32, Expr/binary, 32>>} | Acc]);
do_parse_tokens([{dynamic, {<<" ">>, <<".">>}, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{{expr, 'end'}, <<32, Expr/binary>>} | Acc]);
do_parse_tokens([{dynamic, {<<"#">>, <<".">>}, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{comment, Expr} | Acc]);
do_parse_tokens([{dynamic, Marker, Expr} | Tokens], Acc) ->
    do_parse_tokens(Tokens, [{unknown, Marker, Expr} | Acc]);
do_parse_tokens([], Acc) ->
    lists:reverse(Acc).

resolve_parsed_tokens([Token | _] = Tokens) ->
    do_resolve_parsed_tokens(Tokens, Token, root, {[], []}).

do_resolve_parsed_tokens([{text, Text} = Token | Tokens], _Previous, root, {Static, Dynamic}) ->
    do_resolve_parsed_tokens(Tokens, Token, root, {[Text | Static], Dynamic});
do_resolve_parsed_tokens(
    [{text, Text} = Token | Tokens], _Previous, expr, {Static, [{EFun, EBody0} | Dynamic]}
) ->
    EBody =
        case EBody0 of
            [] -> [Text];
            [BodyHead | BodyTail] -> [<<BodyHead/binary, Text/binary>> | BodyTail]
        end,
    do_resolve_parsed_tokens(Tokens, Token, expr, {Static, [{EFun, EBody} | Dynamic]});
do_resolve_parsed_tokens([{comment, _Comment} = Token | Tokens], _Previous, In, Acc) ->
    do_resolve_parsed_tokens(Tokens, Token, In, Acc);
do_resolve_parsed_tokens(
    [{{expr, inline}, Expr} = Token | Tokens], Previous, root, {Static0, Dynamic}
) ->
    Static =
        case Previous of
            {{expr, _}, _} -> [<<>> | Static0];
            _ -> Static0
        end,
    do_resolve_parsed_tokens(Tokens, Token, root, {Static, [Expr | Dynamic]});
do_resolve_parsed_tokens(
    [{{expr, inline}, Expr0} = Token | Tokens], Previous, expr, {Static, [{EFun, EBody0} | Dynamic]}
) ->
    Expr = <<"<%=", Expr0/binary, ".%>">>,
    EBody =
        case Previous of
            {text, _Text} ->
                case EBody0 of
                    [] -> [Expr];
                    [BodyHead | BodyTail] -> [<<BodyHead/binary, Expr/binary>> | BodyTail]
                end;
            {{expr, _}, _} ->
                [Expr | EBody0]
        end,
    do_resolve_parsed_tokens(Tokens, Token, expr, {Static, [{EFun, EBody} | Dynamic]});
do_resolve_parsed_tokens(
    [{{expr, start}, Expr} = Token | Tokens], Previous, root, {Static0, Dynamic}
) ->
    Static =
        case Previous of
            {{expr, _}, _} -> [<<>> | Static0];
            _ -> Static0
        end,
    do_resolve_parsed_tokens(Tokens, Token, expr, {Static, [{[Expr], []} | Dynamic]});
do_resolve_parsed_tokens(
    [{{expr, continue}, Expr} = Token | Tokens],
    Previous,
    expr,
    {Static, [{[EFunHead | EFunTail] = EFun0, EBody} | Dynamic]}
) ->
    EFun =
        case Previous of
            {{expr, start}, _} -> [<<EFunHead/binary, Expr/binary>> | EFunTail];
            {{expr, continue}, _} -> [<<EFunHead/binary, Expr/binary>> | EFunTail];
            _ -> [Expr | EFun0]
        end,
    do_resolve_parsed_tokens(Tokens, Token, expr, {Static, [{EFun, EBody} | Dynamic]});
do_resolve_parsed_tokens(
    [{{expr, 'end'}, Expr} = Token | Tokens],
    Previous,
    expr,
    {Static, [{[EFunHead | EFunTail] = EFun0, EBody} | Dynamic]}
) ->
    EFun =
        case Previous of
            {{expr, start}, _} -> [<<EFunHead/binary, Expr/binary>> | EFunTail];
            {{expr, continue}, _} -> [<<EFunHead/binary, Expr/binary>> | EFunTail];
            _ -> [Expr | EFun0]
        end,
    do_resolve_parsed_tokens(
        Tokens, Token, root, {Static, [{lists:reverse(EFun), lists:reverse(EBody)} | Dynamic]}
    );
do_resolve_parsed_tokens([Token | Tokens], _Previous, In, Acc) ->
    do_resolve_parsed_tokens(Tokens, Token, In, Acc);
do_resolve_parsed_tokens([], _Token, _In, {Static, Dynamic}) ->
    {lists:reverse(Static), lists:reverse(Dynamic)}.

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
    case io_lib:char_list(List) of
        true ->
            erlang:list_to_binary(List);
        false ->
            BinList = lists:map(fun to_binary/1, List),
            erlang:iolist_to_binary([<<"[">>, lists:join(", ", BinList), <<"]">>])
    end;
to_binary(Fun, _) when is_function(Fun) ->
    Clauses =
        case erlang:fun_info(Fun, env) of
            {env, [{_, _, _, Abstract}]} -> Abstract;
            {env, [{_, _, _, _, _, Abstract}]} -> Abstract
        end,
    AbstractExpr = {'fun', 1, {'clauses', Clauses}},
    IoList = erl_pp:expr(AbstractExpr),
    erlang:iolist_to_binary(IoList);
% to_binary(Map, get) when is_map(Map) ->
%     map_to_bin_with_get_mark(Map);
to_binary(Map, _) when is_map(Map) ->
    map_to_bin_with_put_mark(Map);
to_binary(Tuple, undefined) when is_tuple(Tuple) ->
    to_binary(erlang:tuple_to_list(Tuple));
to_binary(_, _) ->
    <<>>.

map_self_to_bin_with_get_mark(Map) ->
    KVTransform =
        fun(K, _V) ->
            Key = key_to_erl_bin(K),
            Value = string:titlecase(to_binary(K)),
            map_kv_to_binary_with_get_mark(Key, Value)
        end,
    map_self_to_binary(Map, KVTransform).

% map_self_to_bin_with_put_mark(Map) ->
%     KVTransform =
%         fun(K, _V) ->
%             Key = key_to_erl_bin(K),
%             Value = string:titlecase(to_binary(K)),
%             map_kv_to_binary_with_put_mark(Key, Value)
%         end,
%     map_self_to_binary(Map, KVTransform).

map_self_to_binary(Map, KVTransform) ->
    KeyMap = maps:map(fun(K, _) -> K end, Map),
    map_to_binary(KeyMap, KVTransform).

% map_to_bin_with_get_mark(Map) ->
%     KVTransform =
%         fun(K, V) ->
%             Key = key_to_erl_bin(K),
%             Value = value_to_erl_bin(V),
%             map_kv_to_binary_with_get_mark(Key, Value)
%         end,
%     map_to_binary(Map, KVTransform).

map_to_bin_with_put_mark(Map) ->
    KVTransform =
        fun(K, V) ->
            Key = key_to_erl_bin(K),
            Value = value_to_erl_bin(V),
            map_kv_to_binary_with_put_mark(Key, Value)
        end,
    map_to_binary(Map, KVTransform).

map_to_binary(Map, KVTransform) when is_function(KVTransform, 2) ->
    KVs =
        lists:foldl(
            fun({K, V}, Acc) -> [KVTransform(K, V) | Acc] end,
            [],
            maps:to_list(Map)
        ),
    erlang:iolist_to_binary([<<"#{">>, lists:join(", ", lists:reverse(KVs)), <<"}">>]).

map_kv_to_binary_with_get_mark(Key, Value) ->
    map_kv_to_binary(Key, <<":=">>, Value).

map_kv_to_binary_with_put_mark(Key, Value) ->
    map_kv_to_binary(Key, <<"=>">>, Value).

map_kv_to_binary(Key, Sign, Value) ->
    <<Key/binary, 32, Sign/binary, 32, Value/binary>>.

key_to_erl_bin(Atom) when is_atom(Atom) ->
    erl_quote_atom(Atom);
key_to_erl_bin(Bin) when is_binary(Bin) ->
    erl_quote_bin(Bin);
key_to_erl_bin(X) ->
    to_binary(X).

value_to_erl_bin(Bin) when is_binary(Bin) ->
    erl_quote_bin(Bin);
value_to_erl_bin(X) ->
    to_binary(X).

erl_quote_atom(Atom) ->
    Bin = erlang:atom_to_binary(Atom),
    <<"'", Bin/binary, "'">>.

erl_quote_bin(Bin) ->
    <<"<<\"", Bin/binary, "\">>">>.

build_render_fun(Mod0, Fun0, Body0, Args0) ->
    Mod = atom_to_binary(Mod0),
    Fun = atom_to_binary(Fun0),
    Body = erl_quote_bin(Body0),
    Args = map_to_bin_with_put_mark(Args0),
    do_build_render_fun(Mod, Fun, Body, Args).

do_build_render_fun(Mod, Fun, Body, Args) ->
    <<Mod/binary, ":", Fun/binary, "(", Body/binary, ", ", Args/binary, ")">>.

merge_bin_lists(Static, Dynamic) when
    is_list(Static), is_list(Dynamic), length(Static) >= length(Dynamic)
->
    do_merge_bin_lists(Static, Dynamic, <<>>).

do_merge_bin_lists([S | Static], [D | Dynamic], Acc) ->
    do_merge_bin_lists(Static, Dynamic, <<Acc/binary, S/binary, D/binary>>);
do_merge_bin_lists([S | Static], [], Acc) ->
    do_merge_bin_lists(Static, [], <<Acc/binary, S/binary>>);
do_merge_bin_lists([], [], Acc) ->
    Acc.

resolve_expr_fun(Mod, RenderFun, Bindings, {EFun, EBody0}) ->
    Expr = erlang:iolist_to_binary(EFun),
    ExprArgs = resolve_expr_vars(Expr, Bindings),
    Args = maps:merge(Bindings, ExprArgs),
    EBody =
        lists:map(
            fun(Body) -> build_render_fun(Mod, RenderFun, Body, Args) end,
            EBody0
        ),
    merge_bin_lists(EFun, EBody).

resolve_expr(_Mod, _RenderFun, _Bindings, Expr) when is_binary(Expr) ->
    Expr;
resolve_expr(Mod, RenderFun, Bindings, {EFun, EBody}) when is_list(EFun), is_list(EBody) ->
    resolve_expr_fun(Mod, RenderFun, Bindings, {EFun, EBody}).

wrap_expr(Mod, RenderFun, Bindings, Expr) ->
    Args = map_self_to_bin_with_get_mark(Bindings),
    Body = resolve_expr(Mod, RenderFun, Bindings, Expr),
    <<"fun(", Args/binary, ") -> ", Body/binary, " end.">>.

map_dynamic_to_render_fun(Dynamic, Bindings) ->
    lists:map(
        fun(Expr) -> wrap_expr(?MODULE, render, Bindings, Expr) end,
        Dynamic
    ).

resolve_expr_vars(Expr, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Expr)),
    get_tokens_vars(Tokens, Bindings).

get_tokens_vars(Tokens, Bindings) ->
    do_get_tokens_vars(Tokens, Bindings).

do_get_tokens_vars([{var, _, K}, {'=', _}, {var, _, Var} | Tokens], Acc) ->
    V = maps:get(Var, Acc),
    do_get_tokens_vars(Tokens, Acc#{K => V});
do_get_tokens_vars([{var, _, K}, {'=', _}, {'fun', _, Fun} | Tokens], Acc) ->
    do_get_tokens_vars(Tokens, Acc#{K => Fun});
do_get_tokens_vars([{var, _, K}, {'=', _}, {_, _, V} | Tokens], Acc) ->
    do_get_tokens_vars(Tokens, Acc#{K => V});
do_get_tokens_vars([{_, _, K}, {':=', _}, {_, _, V} | Tokens], Acc) ->
    do_get_tokens_vars(Tokens, Acc#{K => V});
do_get_tokens_vars([_ | Tokens], Acc) ->
    do_get_tokens_vars(Tokens, Acc);
do_get_tokens_vars([], Acc) ->
    Acc.

eval(Expr, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Expr)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Exprs, Bindings),
    nomalize_resolved_eval_result(resolve_eval_result(Result(Bindings))).

resolve_eval_result(Result) when is_function(Result, 0) -> Result();
resolve_eval_result(Result) -> Result.

nomalize_resolved_eval_result(Result) when is_list(Result) ->
    erlang:iolist_to_binary(Result);
nomalize_resolved_eval_result(Result) ->
    to_binary(Result).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-define(HTML, <<
    "<html>"
    "<head>"
    "<title><%= Title .%></title>"
    "</head>"
    "<body>"
    "<%# This is a comment and will be ignored .%>"
    "<%= Mod:format([$~, $s], [Title]) .%>"
    "<%= fun() -> Foo = 1, Bar = Foobar, %>"
    "<p><%= Foo .%></p><%= Mod:format([$~, $s], [Bar]) .%>"
    "<% end .%>"
    "<%= case Title of %>"
    "<% <<\"EEL\">> -> %>"
    "<p><%= Title .%></p>"
    "<% ; #{<<\"EEL\">> := EEL} -> %>"
    "<%= Mod:format([$~, $s], [EEL]) .%>"
    "<% end .%>"
    "<%= Title .%>"
    "<div>List below</div>"
    "<%= lists:map(fun(#{foo := Foo}) -> %>"
    "<div><%= Foo .%></div>"
    "<% end, List) .%>"
    "</body>"
    "</html>"
>>).

tokens_test() ->
    Expected = [
        {static, <<"<html><head><title>">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Title ">>},
        {static, <<"</title></head><body>">>},
        {dynamic, {<<"#">>, <<".">>}, <<" This is a comment and will be ignored ">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Mod:format([$~, $s], [Title]) ">>},
        {dynamic, {<<"=">>, <<" ">>}, <<" fun() -> Foo = 1, Bar = Foobar,">>},
        {static, <<"<p>">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Foo ">>},
        {static, <<"</p>">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Mod:format([$~, $s], [Bar]) ">>},
        {dynamic, {<<" ">>, <<".">>}, <<"end ">>},
        {dynamic, {<<"=">>, <<" ">>}, <<" case Title of">>},
        {dynamic, {<<" ">>, <<" ">>}, <<"<<\"EEL\">> ->">>},
        {static, <<"<p>">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Title ">>},
        {static, <<"</p>">>},
        {dynamic, {<<" ">>, <<" ">>}, <<"; #{<<\"EEL\">> := EEL} ->">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Mod:format([$~, $s], [EEL]) ">>},
        {dynamic, {<<" ">>, <<".">>}, <<"end ">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Title ">>},
        {static, <<"<div>List below</div>">>},
        {dynamic, {<<"=">>, <<" ">>}, <<" lists:map(fun(#{foo := Foo}) ->">>},
        {static, <<"<div>">>},
        {dynamic, {<<"=">>, <<".">>}, <<" Foo ">>},
        {static, <<"</div>">>},
        {dynamic, {<<" ">>, <<".">>}, <<"end, List) ">>},
        {static, <<"</body></html>">>}
    ],
    ?assertEqual(Expected, tokens(?HTML)).

parse_tokens_test() ->
    Expected = [
        {text, <<"<html><head><title>">>},
        {{expr, inline}, <<" Title ">>},
        {text, <<"</title></head><body>">>},
        {comment, <<" This is a comment and will be ignored ">>},
        {{expr, inline}, <<" Mod:format([$~, $s], [Title]) ">>},
        {{expr, start}, <<" fun() -> Foo = 1, Bar = Foobar, ">>},
        {text, <<"<p>">>},
        {{expr, inline}, <<" Foo ">>},
        {text, <<"</p>">>},
        {{expr, inline}, <<" Mod:format([$~, $s], [Bar]) ">>},
        {{expr, 'end'}, <<" end ">>},
        {{expr, start}, <<" case Title of ">>},
        {{expr, continue}, <<" <<\"EEL\">> -> ">>},
        {text, <<"<p>">>},
        {{expr, inline}, <<" Title ">>},
        {text, <<"</p>">>},
        {{expr, continue}, <<" ; #{<<\"EEL\">> := EEL} -> ">>},
        {{expr, inline}, <<" Mod:format([$~, $s], [EEL]) ">>},
        {{expr, 'end'}, <<" end ">>},
        {{expr, inline}, <<" Title ">>},
        {text, <<"<div>List below</div>">>},
        {{expr, start}, <<" lists:map(fun(#{foo := Foo}) -> ">>},
        {text, <<"<div>">>},
        {{expr, inline}, <<" Foo ">>},
        {text, <<"</div>">>},
        {{expr, 'end'}, <<" end, List) ">>},
        {text, <<"</body></html>">>}
    ],
    ?assertEqual(Expected, parse_tokens(tokens(?HTML))).

resolve_parsed_tokens_test() ->
    Expected = {
        [
            <<"<html><head><title>">>,
            <<"</title></head><body>">>,
            <<>>,
            <<>>,
            <<>>,
            <<"<div>List below</div>">>,
            <<"</body></html>">>
        ],
        [
            <<" Title ">>,
            <<" Mod:format([$~, $s], [Title]) ">>,
            {
                [<<" fun() -> Foo = 1, Bar = Foobar, ">>, <<" end ">>],
                [<<"<p><%= Foo .%></p><%= Mod:format([$~, $s], [Bar]) .%>">>]
            },
            {
                [
                    <<" case Title of  <<\"EEL\">> -> ">>,
                    <<" ; #{<<\"EEL\">> := EEL} -> ">>,
                    <<" end ">>
                ],
                [<<"<p><%= Title .%></p>">>, <<"<%= Mod:format([$~, $s], [EEL]) .%>">>]
            },
            <<" Title ">>,
            {
                [<<" lists:map(fun(#{foo := Foo}) -> ">>, <<" end, List) ">>],
                [<<"<div><%= Foo .%></div>">>]
            }
        ]
    },
    ?assertEqual(Expected, resolve_parsed_tokens(parse_tokens(tokens(?HTML)))).

to_binary_test() ->
    [
        ?assertEqual(<<"foo">>, to_binary(<<"foo">>)),
        ?assertEqual(<<"foo">>, to_binary(foo)),
        ?assertEqual(<<"0.0">>, to_binary(0.0, [short])),
        ?assertEqual(<<"0">>, to_binary(0)),
        ?assertEqual(<<"foo">>, to_binary("foo")),
        ?assertEqual(<<"[foo, bar, 0]">>, to_binary([foo, <<"bar">>, 0])),
        ?assertEqual(<<"[foo, bar, 0]">>, to_binary({foo, <<"bar">>, 0})),
        ?assertEqual(<<>>, to_binary([]))
    ].

map_self_to_bin_with_get_mark_test() ->
    ?assertEqual(<<"#{'Foo' := Foo}">>, map_self_to_bin_with_get_mark(#{'Foo' => foo})).

% map_self_to_bin_with_put_mark_test() ->
%     ?assertEqual(<<"#{'Foo' => Foo}">>, map_self_to_bin_with_put_mark(#{'Foo' => foo})).

map_kv_to_binary_test() ->
    ?assertEqual(
        <<"Foo => 0">>,
        map_kv_to_binary(<<"Foo">>, <<"=>">>, <<"0">>)
    ).

erl_quote_atom_test() ->
    ?assertEqual(<<"'Foo'">>, erl_quote_atom('Foo')).

erl_quote_bin_test() ->
    ?assertEqual(<<"<<\"Foo\">>">>, erl_quote_bin(<<"Foo">>)).

build_render_fun_test() ->
    ?assertEqual(
        <<"mod:render(<<\"<div><%= Foo .%></div>\">>, #{'Foo' => foo})">>,
        build_render_fun(mod, render, <<"<div><%= Foo .%></div>">>, #{'Foo' => foo})
    ).

merge_bin_lists_test() ->
    ?assertEqual(
        <<"Hello, World!">>,
        merge_bin_lists([<<"Hello">>, <<"Worl">>, <<"!">>], [<<", ">>, <<"d">>])
    ).

resolve_expr_fun_test() ->
    Fun = {
        [<<" case Title of  <<\"EEL\">> -> ">>, <<" ; #{<<\"EEL\">> := EEL} -> ">>, <<" end ">>],
        [<<"<p><%= Title .%></p>">>, <<"<%= Mod:format([$~, $s], [EEL]) .%>">>]
    },
    Expected = <<
        " case Title of  <<\"EEL\">> -> "
        "mod:render(<<\"<p><%= Title .%></p>\">>, #{'Title' => <<\"EEL\">>})"
        " ; #{<<\"EEL\">> := EEL} -> "
        "mod:render(<<\"<%= Mod:format([$~, $s], [EEL]) .%>\">>, #{'Title' => <<\"EEL\">>})"
        " end "
    >>,
    ?assertEqual(Expected, resolve_expr_fun(mod, render, #{'Title' => <<"EEL">>}, Fun)).

wrap_expr_test() ->
    ?assertEqual(
        <<"fun(#{'Foo' := Foo, <<\"Bar\">> := Bar}) -> Foo end.">>,
        wrap_expr(mod, render, #{'Foo' => foo, <<"Bar">> => 0}, <<"Foo">>)
    ).

map_dynamic_to_render_fun_test() ->
    Expected = [
        <<"fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) ->  Title  end.">>,
        <<"fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) ->  Mod:format([$~, $s], [Title])  end.">>,
        <<
            "fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) -> "
            " fun() -> Foo = 1, Bar = Foobar, "
            "eel:render(<<\"<p><%= Foo .%></p><%= Mod:format([$~, $s], [Bar]) .%>\">>, #{'Bar' => foobar, 'Foo' => 1, 'Foobar' => foobar, 'List' => [#{'foo' => foo}, #{'foo' => bar}], 'Mod' => io_lib, 'Title' => <<\"EEL\">>})"
            " end "
            " end."
        >>,
        <<
            "fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) -> "
            " case Title of  <<\"EEL\">> -> "
            "eel:render(<<\"<p><%= Title .%></p>\">>, #{'Foobar' => foobar, 'List' => [#{'foo' => foo}, #{'foo' => bar}], 'Mod' => io_lib, 'Title' => <<\"EEL\">>})"
            " ; #{<<\"EEL\">> := EEL} -> "
            "eel:render(<<\"<%= Mod:format([$~, $s], [EEL]) .%>\">>, #{'Foobar' => foobar, 'List' => [#{'foo' => foo}, #{'foo' => bar}], 'Mod' => io_lib, 'Title' => <<\"EEL\">>})"
            " end "
            " end."
        >>,
        <<"fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) ->  Title  end.">>,
        <<
            "fun(#{'Foobar' := Foobar, 'List' := List, 'Mod' := Mod, 'Title' := Title}) -> "
            " lists:map(fun(#{foo := Foo}) -> "
            "eel:render(<<\"<div><%= Foo .%></div>\">>, #{'Foobar' => foobar, 'List' => [#{'foo' => foo}, #{'foo' => bar}], 'Mod' => io_lib, 'Title' => <<\"EEL\">>, 'foo' => Foo})"
            " end, List) "
            " end."
        >>
    ],
    Bindings = #{
        'Title' => <<"EEL">>,
        'Mod' => io_lib,
        'Foobar' => foobar,
        'List' => [#{foo => foo}, #{foo => bar}]
    },
    {_, Dynamic} = compile(?HTML),
    ?assertEqual(
        Expected,
        map_dynamic_to_render_fun(Dynamic, Bindings)
    ).

resolve_expr_vars_test() ->
    ?assertEqual(
        #{foo => foo, 'Bar' => bar, foobar => foobar},
        resolve_expr_vars(<<"fun(#{foo := foo}) -> Bar = bar end.">>, #{foobar => foobar})
    ).

compile_test() ->
    Expected = {
        [
            <<"<html><head><title>">>,
            <<"</title></head><body>">>,
            <<>>,
            <<>>,
            <<>>,
            <<"<div>List below</div>">>,
            <<"</body></html>">>
        ],
        [
            <<" Title ">>,
            <<" Mod:format([$~, $s], [Title]) ">>,
            {
                [
                    <<" fun() -> Foo = 1, Bar = Foobar, ">>,
                    <<" end ">>
                ],
                [
                    <<"<p><%= Foo .%></p><%= Mod:format([$~, $s], [Bar]) .%>">>
                ]
            },
            {
                [
                    <<" case Title of  <<\"EEL\">> -> ">>,
                    <<" ; #{<<\"EEL\">> := EEL} -> ">>,
                    <<" end ">>
                ],
                [
                    <<"<p><%= Title .%></p>">>,
                    <<"<%= Mod:format([$~, $s], [EEL]) .%>">>
                ]
            },
            <<" Title ">>,
            {
                [
                    <<" lists:map(fun(#{foo := Foo}) -> ">>,
                    <<" end, List) ">>
                ],
                [
                    <<"<div><%= Foo .%></div>">>
                ]
            }
        ]
    },
    ?assertEqual(Expected, compile(?HTML)).

eval_test() ->
    ?assertEqual(
        <<"bar">>,
        eval(<<"fun(#{'Foo' := bar}) -> Foo end.">>, #{'Foo' => bar})
    ).

render_test() ->
    Expected = <<
        "<html>"
        "<head>"
        "<title>EEL</title>"
        "</head>"
        "<body>"
        "EEL"
        "<p>1</p>foobar"
        "<p>EEL</p>"
        "EEL"
        "<div>List below</div>"
        "<div>foo</div>"
        "<div>bar</div>"
        "</body>"
        "</html>"
    >>,
    Bindings = #{
        'Title' => <<"EEL">>,
        'Mod' => io_lib,
        'Foobar' => foobar,
        'List' => [#{foo => foo}, #{foo => bar}]
    },
    ?assertEqual(Expected, render(?HTML, Bindings)).

-endif.
