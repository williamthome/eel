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
         handle_expr/5, handle_text/4, handle_body/1,
         handle_compile/2, handle_ast/1]).

%% Includes
-include("eel_engine.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(text(Index, Pos, Text),        {Index, {text, Pos, Text}}).
-define(expr(Index, Pos, Expr),        {Index, {expr, Pos, Expr}}).
-define(start_expr(Index, Pos, Expr),  {Index, {start_expr, Pos, Expr}}).
-define(mid_expr(Index, Pos, Expr),    {Index, {mid_expr, Pos, Expr}}).
-define(end_expr(Index, Pos, Expr),    {Index, {end_expr, Pos, Expr}}).
-define(nested_expr(Index, Pos, Expr), {Index, {nested_expr, Pos, Expr}}).
-define(comment(Index, Pos, Expr),     {Index, {comment, Pos, Expr}}).
-define(code(Index, Pos, Expr),        {Index, {code, Pos, Expr}}).

%% Types
-type token_name() :: text
                    | expr
                    | start_expr
                    | mid_expr
                    | end_expr
                    | nested_expr
                    | comment
                    | code.
-type token()      :: {token_name(), binary()}.
-type ast()        :: eel_engine:ast().
-type static()     :: eel_engine:static().
-type dynamic()    :: list().

%% States
-record(state, {
    opts = #{} :: map(),
    acc  = []  :: [token() | ast()]
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

markers() -> [{expr,       {"<%=", ".%>"}},
              {start_expr, {"<%=",  "%>"}},
              {mid_expr,   {"<%",   "%>"}},
              {end_expr,   {"<%",  ".%>"}},
              {code,       {"<%:", ":%>"}},
              {comment,    {"<%%", "%%>"}}].

init(Opts) ->
    #state{opts = Opts}.

%% tokenize callbacks

handle_expr(Index, Pos, expr, Expr, State) ->
    push(?expr(Index, Pos, Expr), State);
handle_expr(Index, Pos, start_expr, Expr, State) ->
    push(?start_expr(Index, Pos, Expr), State);
handle_expr(Index, Pos, mid_expr, Expr, State) ->
    push(?mid_expr(Index, Pos, Expr), State);
handle_expr(Index, Pos, end_expr, Expr, State) ->
    push(?end_expr(Index, Pos, Expr), State);
handle_expr(Index, Pos, code, Expr, State) ->
    push(?code(Index, Pos, Expr), State);
handle_expr(Index, Pos, comment, Expr, State) ->
    push(?comment(Index, Pos, Expr), State);
handle_expr(_Index, Pos, Marker, Expr, _State) ->
    ?unknown_marker_error({Pos, Marker, Expr}).

handle_text(Index, Pos, Text, State) ->
    push(?text(Index, Pos, Text), State).

handle_body(#state{acc = Tokens}) ->
    {ok, parse_tokens_to_sd(lists:reverse(Tokens))}.

%% compile callbacks

handle_compile(Token, #state{opts = Opts} = State) ->
    case eel_compiler:dynamic_to_ast(compile(Token, Opts)) of
        {ok, AST} ->
            {ok, push(AST, State)};
        {error, Reason} ->
            {error, Reason}
    end.

handle_ast(#state{acc = AST}) ->
    {ok, lists:reverse(AST)}.

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
-spec parse_tokens_to_sd(Tokens) -> Result when
    Tokens :: [token()],
    Result :: {static(), dynamic()}.

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
do_parse_tokens_to_sd_1([{_, {expr, _, _}} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1([{_, {expr, _, _}} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Multiline expression
do_parse_tokens_to_sd_1([{_, {start_expr, _, _}} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H] | D]});
do_parse_tokens_to_sd_1([{_, {start_expr, _, _}} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
do_parse_tokens_to_sd_1([{_, {mid_expr, _, _}} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
do_parse_tokens_to_sd_1([{_, {end_expr, _, _}} = H | T], in_expr, _, {S, [HD | D]}) ->
    {T, H, {S, [lists:reverse([H | HD]) | D]}};
% Text
do_parse_tokens_to_sd_1([{Index, {text, Pos, Text}} | T], in_text, {PrevIndex, {text, PrevPos, PrevText}}, {[{PrevIndex, {PrevPos, PrevText}} | S], D}) ->
    Concat = <<PrevText/binary, Text/binary>>,
    do_parse_tokens_to_sd_2(T, {Index, {text, Pos, Concat}}, in_text, {Index, {text, Pos, Concat}}, {[{Index, {Pos, Concat}} | S], D});
do_parse_tokens_to_sd_1([{Index, {text, Pos, Text}} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {[{Index, {Pos, Text}} | S], D});
do_parse_tokens_to_sd_1([{_, {text, _, _}} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
% Comment
do_parse_tokens_to_sd_1([{_, {comment, _, _}} | T], In, Prev, {S, D}) ->
    do_parse_tokens_to_sd_1(T, In, Prev, {S, D});
% Debug
do_parse_tokens_to_sd_1([{_, {code, _, _}} = H | T], in_text, Prev, {S, D}) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1([{_, {code, _, _}} = H | T], in_expr, Prev, {S, [HD | D]}) ->
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

should_push_empty_static(_, Curr, {[], _}) ->
    is_expr(Curr);
should_push_empty_static(Prev, Curr, {_, _}) ->
    is_expr(Prev) andalso is_expr(Curr).

is_expr({_, {Name, _, _}}) when Name =:= expr;
                                Name =:= start_expr;
                                Name =:= end_expr;
                                Name =:= nested_expr;
                                Name =:= code ->
    true;
is_expr(_) ->
    false.

% Nested
parse_nested_sd([{Index, {_, Pos, _}} | _] = T, Prev, {S, [HD | D]}) ->
    {Tokens, Nested} = parse_nested_sd_1(T, Prev, {[], []}),
    H = ?nested_expr(Index, Pos, Nested),
    do_parse_tokens_to_sd_1(Tokens, in_expr, H, {S, [[H | HD] | D]}).

parse_nested_sd_1([{_, {start_expr, _, _}} | _] = T, Prev, {S, D}) ->
    {Tokens, _, {S1, [H]}} = do_parse_tokens_to_sd_1(T, in_text, Prev, {S, []}),
    parse_nested_sd_1(Tokens, H, {S1, [H | D]});
parse_nested_sd_1([{_, {mid_expr, _, _}} | _] = T, _, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse(D)}};
parse_nested_sd_1([{_, {end_expr, _, _}} | _] = T, _, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse(D)}};
parse_nested_sd_1([{Index, {text, Pos, Text}} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {[{Index, {Pos, Text}} | S], D});
parse_nested_sd_1([{_, {expr, _, _}} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {S, [H | D]});
parse_nested_sd_1([{_, {code, _, _}} = H | T], _, {S, D}) ->
    parse_nested_sd_1(T, H, {S, [H | D]});
parse_nested_sd_1([{_, {comment, _, _}} | T], Prev, {S, D}) ->
    parse_nested_sd_1(T, Prev, {S, D}).

%% -----------------------------------------------------------------------------
%% @private
%% @doc Compile.
%% @end
%% -----------------------------------------------------------------------------

compile({Index, {expr, Pos, Expr}}, _) ->
    {Index, {Pos, wrap_expr(Expr)}};
compile({Index, {start_expr, Pos, Expr}}, _) ->
    {Index, {Pos, wrap_expr_begin(Expr)}};
compile({Index, {mid_expr, Pos, Expr}}, _) when is_binary(Expr) ->
    {Index, {Pos, <<32, Expr/binary, 32>>}};
compile({Index, {end_expr, Pos, Expr}}, _) ->
    {Index, {Pos, wrap_expr_end(Expr)}};
compile(Tokens, Opts) when is_list(Tokens) ->
    [{Index, {_, Pos, _}} | _] = Tokens,
    Expr =
        lists:map(
            fun(Token) ->
                {_, {_, Bin}} = compile(Token, Opts),
                Bin
            end,
            Tokens
        ),
    {Index, {Pos, Expr}};
compile({Index, {nested_expr, Pos, Tokens}}, Opts) ->
    Expr0 = zip_compile(Tokens, Opts),
    Expr1 = lists:join(", ", Expr0),
    Expr = erlang:iolist_to_binary([" erlang:iolist_to_binary([", Expr1, "])"]),
    {Index, {Pos, wrap_expr(Expr)}};
compile({Index, {code, Pos, Expr}}, _) ->
    {Index, {Pos, wrap_expr(<<Expr/binary, ", <<>>">>)}}.

zip_compile(Tokens, Opts) ->
    lists:map(
        fun
            ({_, {_, Bin}}) when is_binary(Bin) ->
                <<"<<\"", Bin/binary, "\">>">>;
            (Token) ->
                {_, {_, Bin}} = compile(Token, Opts),
                Bin
        end,
        eel_evaluator:zip(Tokens)
    ).

wrap_expr_begin(Expr) ->
    erlang:iolist_to_binary([" eel_converter:to_binary(fun() -> ", Expr]).

wrap_expr_end(Expr) ->
    erlang:iolist_to_binary([Expr | " end) "]).

wrap_expr(Expr) ->
    wrap_expr_end(wrap_expr_begin(Expr)).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% TODO: Improve tests

handle_expr_test() ->
    [
        {
            "Should return expr token",
            ?assertEqual(
                #state{acc = [{1, {expr, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                #state{acc = [{1, {start_expr, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, start_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                #state{acc = [{1, {mid_expr, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, mid_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                #state{acc = [{1, {end_expr, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, end_expr, <<"Foo">>, #state{})
            )
        },
        {
            "Should return code token",
            ?assertEqual(
                #state{acc = [{1, {code, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, code, <<"Foo">>, #state{})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                #state{acc = [{1, {comment, {1, 1}, <<"Foo">>}}]},
                handle_expr(1, {1, 1}, comment, <<"Foo">>, #state{})
            )
        },
        {
            "Should raise unknown marker error",
            ?assertError(
                unknown_marker,
                handle_expr(1, {1, 1}, unknown, <<"Foo">>, #state{})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                #state{acc = [{1, {text, {1, 1}, <<"Foo">>}}]},
                handle_text(1, {1, 1}, <<"Foo">>, #state{})
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
    Expected = {[<<>>,
                   {2,{{1,10},<<"<h1>Title</h1>">>}},
                   <<>>,<<>>,
                   {27,{{1,301},<<"<footer>Footer</footer>">>}}],
                  [{1,{expr,{1,1},<<"foo">>}},
                   {3,
                    {code,
                     {1,11},
                     <<"io:format(\"Print but not render me!~n\")">>}},
                   [{4,{start_expr,{1,56},<<"case 1 of">>}},
                    {5,{mid_expr,{1,70},<<"2 ->">>}},
                    {6,
                     {nested_expr,
                      {1,78},
                      {[{6,{{1,78},<<"<p>Foo</p>">>}}],[]}}},
                    {7,{mid_expr,{1,79},<<"; Bar ->">>}},
                    {8,
                     {nested_expr,
                      {1,91},
                      {[{8,{{1,91},<<"<p>">>}},{24,{{1,271},<<"</p>">>}}],
                       [[{9,{start_expr,{1,92},<<"case hello =:= world of">>}},
                         {10,{mid_expr,{1,120},<<"true ->">>}},
                         {11,{expr,{1,131},<<"hello">>}},
                         {12,{mid_expr,{1,142},<<"; false ->">>}},
                         {13,
                          {nested_expr,
                           {1,156},
                           {[{13,{{1,156},<<"<p>">>}},
                             <<>>,
                             {22,{{1,262},<<"</p>">>}}],
                            [{15,{code,{1,180},<<"ignore_me">>}},
                             [{16,
                               {start_expr,{1,195},<<"case car =:= bus of">>}},
                              {17,{mid_expr,{1,219},<<"true ->">>}},
                              {18,
                               {nested_expr,
                                {1,230},
                                {[{18,{{1,230},<<"Car">>}}],[]}}},
                              {19,{mid_expr,{1,231},<<"; false ->">>}},
                              {20,{expr,{1,245},<<"bus">>}},
                              {21,{end_expr,{1,254},<<"end">>}}]]}}},
                         {23,{end_expr,{1,263},<<"end">>}}]]}}},
                    {25,{end_expr,{1,272},<<"end">>}}],
                   {26,{expr,{1,280},<<"Foo = foo, Foo">>}}]},
    {ok, Result} = eel_tokenizer:tokenize(Bin, #{engine => ?MODULE}),
    ?assertEqual(Expected, Result).

parse_tokens_to_sd_test() ->
    Tokens =[
        {1,{text,{1,1},<<"<h1>">>}},
        {2,{expr,{1,2},<<"maps:get('Title', Bindings, <<\"EEl\">>)">>}},
        {3,{text,{1,46},<<"</h1>">>}},
        {4,{comment,{1,47},<<"<h2><%= Foo .%></h2>">>}},
        {5,{text,{1,73},<<"<ul>">>}},
        {6,{start_expr,{1,74},<<"lists:map(fun(Item) ->">>}},
        {7,{text,{1,101},<<"<li>">>}},
        {8,{expr,{1,102},<<"Item">>}},
        {9,{text,{1,112},<<"</li>">>}},
        {10,{end_expr,{1,113},<<"end, List)">>}},
        {11,{text,{1,128},<<"</ul>">>}},
        {12,{start_expr,{1,129},<<"Length = erlang:length(List),">>}},
        {13,{text,{1,163},<<"<div>Item count: ">>}},
        {14,{expr,{1,164},<<"Length">>}},
        {15,{text,{1,176},<<"</div>">>}},
        {16,{start_expr,{1,177},<<"case Length > 0 of true ->">>}},
        {17,{text,{1,208},<<"<ul>">>}},
        {18,{start_expr,{1,209},<<"lists:map(fun(N) ->">>}},
        {19,{text,{1,233},<<"<li>">>}},
        {20,{expr,{1,234},<<"N">>}},
        {21,{text,{1,241},<<"</li>">>}},
        {22,{end_expr,{1,242},<<"end, lists:seq(1, Length))">>}},
        {23,{text,{1,273},<<"</ul>">>}},
        {24,{end_expr,{1,274},<<"; false -> <<\"Empty list\">> end">>}},
        {25,{end_expr,{1,310},<<>>}}
    ],
    Expected = {
        [{1,{{1,1},<<"<h1>">>}},
        {5,{{1,73},<<"</h1><ul>">>}},
        {11,{{1,128},<<"</ul>">>}}],
        [{2,
            {expr,
            {1,2},
            <<"maps:get('Title', Bindings, <<\"EEl\">>)">>}},
            [{6,{start_expr,{1,74},<<"lists:map(fun(Item) ->">>}},
            {7,
            {nested_expr,
            {1,101},
            {[{7,{{1,101},<<"<li>">>}},{9,{{1,112},<<"</li>">>}}],
                [{8,{expr,{1,102},<<"Item">>}}]}}},
            {10,{end_expr,{1,113},<<"end, List)">>}}],
            [{12,
            {start_expr,{1,129},<<"Length = erlang:length(List),">>}},
            {13,
            {nested_expr,
            {1,163},
            {[{13,{{1,163},<<"<div>Item count: ">>}},
                {15,{{1,176},<<"</div>">>}}],
                [{14,{expr,{1,164},<<"Length">>}},
                [{16,
                {start_expr,
                    {1,177},
                    <<"case Length > 0 of true ->">>}},
                {17,
                {nested_expr,
                    {1,208},
                    {[{17,{{1,208},<<"<ul>">>}},
                    {23,{{1,273},<<"</ul>">>}}],
                    [[{18,
                        {start_expr,{1,209},<<"lists:map(fun(N) ->">>}},
                    {19,
                        {nested_expr,
                        {1,233},
                        {[{19,{{1,233},<<"<li>">>}},
                        {21,{{1,241},<<"</li>">>}}],
                        [{20,{expr,{1,234},<<"N">>}}]}}},
                    {22,
                        {end_expr,
                        {1,242},
                        <<"end, lists:seq(1, Length))">>}}]]}}},
                {24,
                {end_expr,
                    {1,274},
                    <<"; false -> <<\"Empty list\">> end">>}}]]}}},
            {25,{end_expr,{1,310},<<>>}}]]},
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
    {ok, Snapshot0} = eel:compile(Bin),
    Bindings = #{list => [foo, bar, baz]},
    {ok, Snapshot} = eel_renderer:render(Bindings, Snapshot0),
    Result = eel_evaluator:eval(Snapshot),
    ?assertEqual(Expected, Result).

-endif.
