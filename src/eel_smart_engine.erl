%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl default engine module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([markers/0,
         init/1,
         handle_expr/4, handle_text/3, handle_body/1,
         handle_compile/2, handle_ast/1]).

%% Includes
-include("eel_engine.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(token(Name),    fun(BinOrSD) -> {Name, BinOrSD} end).
-define(text,           ?token(text)).
-define(expr,           ?token(expr)).
-define(start_expr,     ?token(start_expr)).
-define(mid_expr,       ?token(mid_expr)).
-define(end_expr,       ?token(end_expr)).
-define(nested_expr,    ?token(nested_expr)).
-define(comment,        ?token(comment)).
-define(debug,          ?token(debug)).
-define(is_expr(Token), is_tuple(Token) andalso
                        (element(1, Token) =:= expr orelse
                         element(1, Token) =:= start_expr orelse
                         element(1, Token) =:= end_expr orelse
                         element(1, Token) =:= nested_expr orelse
                         element(1, Token) =:= debug)).

%% Types
-type token_name() :: text
                      | expr
                      | start_expr
                      | mid_expr
                      | end_expr
                      | nested_expr
                      | comment
                      | debug.
-type token()      :: {token_name(), binary()}.
-type ast()        :: eel_engine:ast().
-type static()     :: eel_engine:static().
-type dynamic()    :: list().

%% States
-record(state, {
    opts :: map(),
    acc = [] :: [token() | ast()]
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

markers() -> [{expr,       {"<%=", ".%>"}},
              {start_expr, {"<%=",  "%>"}},
              {mid_expr,   {"<%",   "%>"}},
              {end_expr,   {"<%",  ".%>"}},
              {debug,      {"<%:", ":%>"}},
              {comment,    {"<%%", "%%>"}}].

init(Opts) ->
    #state{opts = Opts}.

%% tokenize callbacks

handle_expr(_Pos, expr, Expr, State) ->
    push(?expr(Expr), State);
handle_expr(_Pos, start_expr, Expr, State) ->
    push(?start_expr(Expr), State);
handle_expr(_Pos, mid_expr, Expr, State) ->
    push(?mid_expr(Expr), State);
handle_expr(_Pos, end_expr, Expr, State) ->
    push(?end_expr(Expr), State);
handle_expr(_Pos, debug, Expr, State) ->
    push(?debug(Expr), State);
handle_expr(_Pos, comment, Expr, State) ->
    push(?comment(Expr), State);
handle_expr(Pos, Marker, Expr, _State) ->
    ?unknown_marker_error({Pos, Marker, Expr}).

handle_text(_Pos, Text, State) ->
    push(?text(Text), State).

handle_body(#state{acc = Tokens}) ->
    parse_tokens_to_sd(lists:reverse(Tokens)).

%% compile callbacks

handle_compile(Token, #state{opts = Opts} = State) ->
    push(eel_compiler:dynamic_to_ast(compile(Token, Opts)), State).

handle_ast(#state{acc = AST}) ->
    lists:reverse(AST).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

push(Term, #state{acc = Acc} = State) ->
    State#state{acc = [Term | Acc]}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc Parses tokens to statics and dynamics.
%% @end
%% -----------------------------------------------------------------------------
-spec parse_tokens_to_sd(Tokens :: [token()]) -> {static(), dynamic()}.

parse_tokens_to_sd(Tokens) ->
    {[], SD} = do_parse_tokens_to_sd(Tokens),
    SD.

do_parse_tokens_to_sd(Tokens) ->
    do_parse_tokens_to_sd(Tokens, in_text, undefined, {[], []}).

do_parse_tokens_to_sd(Tokens, In, Prev, SD) ->
    case do_parse_tokens_to_sd_1(Tokens, In, Prev, SD) of
        {[], {S, D}} ->
            {[], {lists:reverse(S), lists:reverse(D)}};
        {RestTokens, Prev1, AccSD} ->
            do_parse_tokens_to_sd(RestTokens, in_text, Prev1, AccSD)
    end.

% Single line expression
do_parse_tokens_to_sd_1([{expr, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1([{expr, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Multiline expression
do_parse_tokens_to_sd_1([{start_expr, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H] | D]});
do_parse_tokens_to_sd_1([{start_expr, _} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
do_parse_tokens_to_sd_1([{mid_expr, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
do_parse_tokens_to_sd_1([{end_expr, _} = H | T], in_expr, _, {S, [HD | D]}) ->
    {T, H, {S, [lists:reverse([H | HD]) | D]}};
% Text
do_parse_tokens_to_sd_1([{text, Text} | T], in_text, {text, PrevText}, {[PrevText | S], D}) ->
    Concat = <<PrevText/binary, Text/binary>>,
    do_parse_tokens_to_sd_2(T, {text, Concat}, in_text, Concat, {[Concat | S], D});
do_parse_tokens_to_sd_1([{text, Text} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {[Text | S], D});
do_parse_tokens_to_sd_1([{text, _} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
% Comment
do_parse_tokens_to_sd_1([{comment, _} | T], In, Prev, {S, D}) ->
    do_parse_tokens_to_sd_1(T, In, Prev, {S, D});
% Debug
do_parse_tokens_to_sd_1([{debug, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1([{debug, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Done
do_parse_tokens_to_sd_1([], _, _, SD) ->
    {[], SD}.

do_parse_tokens_to_sd_2(Tokens, Curr, In, Prev, {S, D}) ->
    SD = case should_push_empty_static(Prev, Curr, {S, D}) of
             true -> {[<<>> | S], D};
             false -> {S, D}
         end,
    do_parse_tokens_to_sd_1(Tokens, In, Curr, SD).

% FIXME: Dialyzer warnings (maybe false positive)
should_push_empty_static(_, Curr, {[], _}) when ?is_expr(Curr) -> true;
should_push_empty_static(Prev, Curr, {_, _}) -> ?is_expr(Prev) andalso
                                                ?is_expr(Curr).

% Nested
parse_nested_sd(T, Prev, {S, [HD | D]}) ->
    {Tokens, Nested} = parse_nested_sd_1(T, Prev, {[], []}),
    H = ?nested_expr(Nested),
    do_parse_tokens_to_sd_1(Tokens, in_expr, H, {S, [[H | HD] | D]}).

parse_nested_sd_1([{start_expr, _} | _] = T, Prev, {S, D}) ->
    {Tokens, _, {S1, [H]}} = do_parse_tokens_to_sd_1(T, in_text, Prev, {S, []}),
    parse_nested_sd_1(Tokens, H, {S1, [H | D]});
parse_nested_sd_1([{mid_expr, _} | _] = T, _, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse(D)}};
parse_nested_sd_1([{end_expr, _} | _] = T, _, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse(D)}};
parse_nested_sd_1([{text, Text} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {[Text | S], D});
parse_nested_sd_1([{expr, _} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {S, [H | D]});
parse_nested_sd_1([{debug, _} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {S, [H | D]});
parse_nested_sd_1([{comment, _} | T], Prev, {S, D}) ->
    parse_nested_sd_1(T, Prev, {S, D}).

%% -----------------------------------------------------------------------------
%% @private
%% @doc Compile.
%% @end
%% -----------------------------------------------------------------------------

compile({expr, Expr}, _) ->
    wrap_expr(Expr);
compile({start_expr, Expr}, _) ->
    wrap_expr_begin(Expr);
compile({mid_expr, Expr}, _) when is_binary(Expr) ->
    <<32, Expr/binary, 32>>;
compile({end_expr, Expr}, _) ->
    wrap_expr_end(Expr);
compile(Tokens, Opts) when is_list(Tokens) ->
    lists:map(fun(Token) -> compile(Token, Opts) end, Tokens);
compile({nested_expr, Tokens}, Opts) ->
    Expr0 = lists:map(fun(S) when is_binary(S) -> <<"<<\"", S/binary, "\">>">>;
                         (Token) -> compile(Token, Opts) end,
                      eel_renderer:zip(Tokens)),
    Expr1 = lists:join(", ", Expr0),
    Expr = erlang:iolist_to_binary([" erlang:iolist_to_binary([", Expr1, "])"]),
    wrap_expr(Expr);
compile({debug, Expr}, _) ->
    wrap_expr(<<Expr/binary, ", <<>>">>).

wrap_expr_begin(Expr) ->
    erlang:iolist_to_binary([" eel_converter:to_binary(fun() -> ", Expr]).

wrap_expr_end(Expr) ->
    erlang:iolist_to_binary([Expr | " end) "]).

wrap_expr(Expr) ->
    wrap_expr_end(wrap_expr_begin(Expr)).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% TODO: Improve tests

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return expr token",
            ?assertEqual(
                #state{acc = [{expr, <<"Foo">>}]},
                handle_expr({1, 1}, expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                #state{acc = [{start_expr, <<"Foo">>}]},
                handle_expr({1, 1}, start_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                #state{acc = [{mid_expr, <<"Foo">>}]},
                handle_expr({1, 1}, mid_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                #state{acc = [{end_expr, <<"Foo">>}]},
                handle_expr({1, 1}, end_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return debug token",
            ?assertEqual(
                #state{acc = [{debug, <<"Foo">>}]},
                handle_expr({1, 1}, debug, <<"Foo">>, #state{})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                #state{acc = [{comment, <<"Foo">>}]},
                handle_expr({1, 1}, comment, <<"Foo">>, #state{})
            )
        },
        {
            "Should raise unknown marker error",
            ?assertError(
                unknown_marker,
                handle_expr({1, 1}, unknown, <<"Foo">>, #state{})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                #state{acc = [{text, <<"Foo">>}]},
                handle_text({1, 1}, <<"Foo">>, #state{})
            )
        }
    ].

handle_body_test() ->
    Bin = <<
        "<%= foo .%>"
        "<h1>Title</h1>"
        "<%: io:format(\"Print but not render me!~n\") :%>"
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %>"
            "<p>"
                "<%= case hello =:= world of %>"
                "<% true -> %>"
                    "<%= hello .%>"
                "<% ; false -> %>"
                    "<p>"
                        "<%% This is a comment %%>"
                        "<%: ignore_me :%>"
                        "<%= case car =:= bus of %>"
                        "<% true -> %>"
                            "Car"
                        "<% ; false -> %>"
                            "<%= bus .%>"
                        "<% end .%>"
                    "</p>"
                "<% end .%>"
            "</p>"
        "<% end .%>"
        "<%= Foo = foo, Foo .%>"
        "<footer>Footer</footer>"
    >>,
    Expected = {[<<>>,<<"<h1>Title</h1>">>,<<>>,<<>>,
                   <<"<footer>Footer</footer>">>],
                  [{expr,<<"foo">>},
                   {debug,<<"io:format(\"Print but not render me!~n\")">>},
                   [{start_expr,<<"case 1 of">>},
                    {mid_expr,<<"2 ->">>},
                    {nested_expr,{[<<"<p>Foo</p>">>],[]}},
                    {mid_expr,<<"; Bar ->">>},
                    {nested_expr,
                        {[<<"<p>">>,<<"</p>">>],
                         [[{start_expr,<<"case hello =:= world of">>},
                           {mid_expr,<<"true ->">>},
                           {expr,<<"hello">>},
                           {mid_expr,<<"; false ->">>},
                           {nested_expr,
                               {[<<"<p>">>,<<>>,<<"</p>">>],
                                [{debug,<<"ignore_me">>},
                                 [{start_expr,<<"case car =:= bus of">>},
                                  {mid_expr,<<"true ->">>},
                                  {nested_expr,{[<<"Car">>],[]}},
                                  {mid_expr,<<"; false ->">>},
                                  {expr,<<"bus">>},
                                  {end_expr,<<"end">>}]]}},
                           {end_expr,<<"end">>}]]}},
                    {end_expr,<<"end">>}],
                   {expr,<<"Foo = foo, Foo">>}]},
    Result = eel_tokenizer:tokenize(Bin, #{engine => ?MODULE}),
    ?assertEqual(Expected, Result).

parse_tokens_to_sd_test() ->
    Tokens = [
        {expr, <<"foo">>},
        {text,<<"<h1>Title</h1>">>},
        {debug,<<"io:format(\"Print but not render me!~n\")">>},
        {start_expr,<<"case 1 of">>},
        {mid_expr,<<"2 ->">>},
        {text,<<"<p>Foo</p>">>},
        {mid_expr,<<"; Bar ->">>},
        {text,<<"<p>">>},
        {start_expr,<<"case hello =:= world of">>},
        {mid_expr,<<"true ->">>},
        {expr,<<"hello">>},
        {mid_expr,<<"; false ->">>},
        {text,<<"<p>">>},
        {comment,<<"This is a comment">>},
        {debug,<<"ignore_me">>},
        {start_expr,<<"case car =:= bus of">>},
        {mid_expr,<<"true ->">>},
        {text,<<"Car">>},
        {mid_expr,<<"; false ->">>},
        {expr,<<"bus">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>},
        {expr,<<"Foo = foo, Foo">>},
        {text,<<"<footer>Footer</footer>">>}
    ],
    Expected = {[<<>>,<<"<h1>Title</h1>">>,<<>>,<<>>,
                   <<"<footer>Footer</footer>">>],
                  [{expr,<<"foo">>},
                   {debug,<<"io:format(\"Print but not render me!~n\")">>},
                   [{start_expr,<<"case 1 of">>},
                    {mid_expr,<<"2 ->">>},
                    {nested_expr,{[<<"<p>Foo</p>">>],[]}},
                    {mid_expr,<<"; Bar ->">>},
                    {nested_expr,
                        {[<<"<p>">>,<<"</p>">>],
                         [[{start_expr,<<"case hello =:= world of">>},
                           {mid_expr,<<"true ->">>},
                           {expr,<<"hello">>},
                           {mid_expr,<<"; false ->">>},
                           {nested_expr,
                               {[<<"<p>">>,<<>>,<<"</p>">>],
                                [{debug,<<"ignore_me">>},
                                 [{start_expr,<<"case car =:= bus of">>},
                                  {mid_expr,<<"true ->">>},
                                  {nested_expr,{[<<"Car">>],[]}},
                                  {mid_expr,<<"; false ->">>},
                                  {expr,<<"bus">>},
                                  {end_expr,<<"end">>}]]}},
                           {end_expr,<<"end">>}]]}},
                    {end_expr,<<"end">>}],
                   {expr,<<"Foo = foo, Foo">>}]},
    Result = parse_tokens_to_sd(Tokens),
    ?assertEqual(Expected, Result).

handle_render_test() ->
    Expected = <<
        "<h1>EEl</h1>"
        "<ul>"
            "<li>foo</li>"
            "<li>bar</li>"
            "<li>baz</li>"
        "</ul>"
        "<div>Item count: 3</div>"
        "<ul>"
            "<li>1</li>"
            "<li>2</li>"
            "<li>3</li>"
        "</ul>"
    >>,
    Bin = <<
        "<h1><%= maps:get('Title', Bindings, <<\"EEl\">>) .%></h1>"
        "<%% <h2><%= Foo .%></h2> %%>"
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
        "<% ; false -> <<\"Empty list\">> end .%>"
        "<%  .%>"
    >>,
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Dynamic),
    Bindings = #{list => [foo, bar, baz]},
    Result = eel_renderer:render({Static, AST}, Bindings),
    ?assertEqual(Expected, Result).

-endif.
