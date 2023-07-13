%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl default engine module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_smart_engine).

-behaviour(eel_engine).

-compile(inline_list_funcs).
-compile({inline, [ handle_expr_start/2
                  , handle_expr_end/3
                  , do_parse_tokens_to_sd/4
                  , do_parse_tokens_to_sd_1/4
                  , do_parse_tokens_to_sd_2/5
                  , compile/1
                  , zip_compile/1
                  ]}).

%% eel_engine callbacks
-export([ init/1
        , handle_expr_start/2
        , handle_expr_end/3
        , handle_text/3
        , handle_body/2
        , handle_compile/2
        , handle_ast/2
        , handle_eval/5
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(nested_expr(Index, Pos, Expr), {Index, {nested_expr, Pos, Expr}}).

-record(state, {}).
% -opaque state() :: #state{}.

%% Types
-type token_name() :: text
                    | expr
                    | start_expr
                    | mid_expr
                    | end_expr
                    | nested_expr
                    | comment
                    | code
                    .
-type token()      :: {token_name(), binary()}.
-type static()     :: eel_engine:static().
-type dynamic()    :: list().
-type index()      :: eel_engine:index().

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init(_Opts) ->
    {ok, #state{}}.

%% tokenize callbacks

handle_expr_start(<<"<%=", 32, Rest/binary>>, State) ->
    {ok, {<<"<%=", 32>>, 4, Rest, State}};
handle_expr_start(<<"<%", 32, Rest/binary>>, State) ->
    {ok, {<<"<%", 32>>, 3, Rest, State}};
handle_expr_start(<<"<%:", 32, Rest/binary>>, State) ->
    {ok, {<<"<%:", 32>>, 4, Rest, State}};
handle_expr_start(<<"<%%", 32, Rest/binary>>, State) ->
    {ok, {<<"<%%", 32>>, 4, Rest, State}};
handle_expr_start(_, State) ->
    {ok, State}.

handle_expr_end(<<"<%=", 32>>, <<32, ".%>", Rest/binary>>, State) ->
    {ok, {expr, 4, Rest, State}};
handle_expr_end(<<"<%=", 32>>, <<32, "%>", Rest/binary>>, State) ->
    {ok, {start_expr, 3, Rest, State}};
handle_expr_end(<<"<%", 32>>, <<32, "%>", Rest/binary>>, State) ->
    {ok, {mid_expr, 3, Rest, State}};
handle_expr_end(<<"<%", 32>>, <<32, ".%>", Rest/binary>>, State) ->
    {ok, {end_expr, 4, Rest, State}};
handle_expr_end(<<"<%%", 32>>, <<32, "%%>", Rest/binary>>, State) ->
    {ok, {comment, 4, Rest, State}};
handle_expr_end(<<"<%:", 32>>, <<32, ":%>", Rest/binary>>, State) ->
    {ok, {code, 4, Rest, State}};
handle_expr_end(_, _, #state{} = State) ->
    {ok, State}.

handle_text(Text, Pos, State) ->
    {ok, {Text, Pos, State}}.

handle_body(Tokens, #state{} = State) ->
    {ok, {parse_tokens_to_sd(Tokens), State}}.

%% compile callbacks

handle_compile(Token, #state{} = State) ->
    {ok, {compile(Token), State}}.

handle_ast(AST, #state{} = State) ->
    {ok, {AST, State}}.

%% runtime callbacks

handle_eval(_Index, AST, Bindings, Opts, State) ->
    LocalFunHandler = maps:get(local_function_handler, Opts, none),
    NonLocalFunHandler = maps:get(non_local_function_handler, Opts, none),
    {value, Binary, NewBindings} = erl_eval:exprs( AST
                                                 , Bindings
                                                 , LocalFunHandler
                                                 , NonLocalFunHandler
                                                 ),
    {ok, {Binary, NewBindings, State}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @private
%% @doc Parses tokens to statics and dynamics.
%% @end
%% -----------------------------------------------------------------------------
-spec parse_tokens_to_sd(Tokens) -> Result
    when Tokens :: [token()]
       , Result :: {static(), dynamic()}
       .

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
do_parse_tokens_to_sd_1( [{_, {expr, _, _}} = H | T]
                       , in_text
                       , Prev
                       , {S, D} ) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1( [{_, {expr, _, _}} = H | T]
                       , in_expr
                       , Prev
                       , {S, [HD | D]} ) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Multiline expression
do_parse_tokens_to_sd_1( [{_, {start_expr, _, _}} = H | T]
                       , in_text
                       , Prev
                       , {S, D} ) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H] | D]});
do_parse_tokens_to_sd_1( [{_, {start_expr, _, _}} | _] = T
                       , in_expr
                       , Prev
                       , SD ) ->
    parse_nested_sd(T, Prev, SD);
do_parse_tokens_to_sd_1( [{_, {mid_expr, _, _}} = H | T]
                       , in_expr
                       , Prev
                       , {S, [HD | D]} ) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
do_parse_tokens_to_sd_1( [{_, {end_expr, _, _}} = H | T]
                       , in_expr
                       , _Prev
                       , {S, [HD | D]} ) ->
    {T, H, {S, [lists:reverse([H | HD]) | D]}};
% Text
do_parse_tokens_to_sd_1( [{Index, {text, Pos, Text}} | T]
                       , in_text
                       , {PrevIndex, {text, PrevPos, PrevText}}
                       , {[{PrevIndex, {text, PrevPos, PrevText}} | S], D} ) ->
    MergedText  = [PrevText, Text],
    MergedToken = {Index, {text, Pos, MergedText}},
    do_parse_tokens_to_sd_2( T
                           , MergedToken
                           , in_text
                           , MergedToken
                           , {[{Index, {text, Pos, MergedText}} | S], D}
                           );
do_parse_tokens_to_sd_1( [{Index, {text, Pos, Text}} = H | T]
                       , in_text
                       , Prev
                       , {S, D} ) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {[{Index, {text, Pos, Text}} | S], D});
do_parse_tokens_to_sd_1( [{_, {text, _, _}} | _] = T
                       , in_expr
                       , Prev
                       , SD ) ->
    parse_nested_sd(T, Prev, SD);
% Comment
do_parse_tokens_to_sd_1( [{_, {comment, _, _}} | T]
                       , In
                       , Prev
                       , {S, D} ) ->
    do_parse_tokens_to_sd_1(T, In, Prev, {S, D});
% Debug
do_parse_tokens_to_sd_1( [{_, {code, _, _}} = H | T]
                       , in_text
                       , Prev
                       , {S, D} ) ->
    do_parse_tokens_to_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_tokens_to_sd_1( [{_, {code, _, _}} = H | T]
                       , in_expr
                       , Prev
                       , {S, [HD | D]} ) ->
    do_parse_tokens_to_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Done
do_parse_tokens_to_sd_1([], _, _, SD) ->
    {[], SD}.

do_parse_tokens_to_sd_2(Tokens, Curr, In, Prev, {S, D}) ->
    SD =
        case should_push_empty_static(Prev, Curr, {S, D}) of
            true ->
                {[<<>> | S], D};
            false ->
                {S, D}
        end,
    do_parse_tokens_to_sd_1(Tokens, In, Curr, SD).

should_push_empty_static(_, Curr, {[], _}) ->
    is_expr(Curr);
should_push_empty_static(Prev, Curr, {_, _}) ->
    is_expr(Prev) andalso is_expr(Curr).

is_expr({_, {MarkerId, _, _}}) when MarkerId =:= expr
                                  ; MarkerId =:= start_expr
                                  ; MarkerId =:= end_expr
                                  ; MarkerId =:= nested_expr
                                  ; MarkerId =:= code ->
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
    parse_nested_sd_1(T, H, {[{Index, {text, Pos, Text}} | S], D});
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

compile({Index, {expr, Pos, Expr}}) ->
    {Index, {expr, Pos, wrap_expr(Expr)}};
compile({Index, {start_expr, Pos, Expr}}) ->
    {Index, {start_expr, Pos, wrap_expr_begin(Expr)}};
compile({Index, {mid_expr, Pos, Expr}}) ->
    {Index, {mid_expr, Pos, [32, Expr, 32]}};
compile({Index, {end_expr, Pos, Expr}}) ->
    {Index, {end_expr, Pos, wrap_expr_end(Expr)}};
compile([{Index, {_, Pos, _}} | _] = Tokens) ->
    Expr =
        lists:map(
            fun(Token) ->
                {_, {_, _, Expr0}} = compile(Token),
                Expr0
            end,
            Tokens
        ),
    {Index, {expr, Pos, Expr}};
compile({Index, {nested_expr, Pos, Tokens}}) ->
    Expr0 = zip_compile(Tokens),
    Expr1 = lists:join(",", Expr0),
    Expr = ["[", Expr1, "]"],
    {Index, {nested_expr, Pos, wrap_expr(Expr)}};
compile({Index, {code, Pos, Expr}}) ->
    {Index, {code, Pos, wrap_expr([Expr, ",<<>>"])}}.

zip_compile(Tokens) ->
    lists:map(
        fun
            ({_, {text, _, Text}}) ->
                ["<<\"", Text, "\"/utf8>>"];
            (Token) ->
                {_, {_, _, Expr}} = compile(Token),
                Expr
        end,
        eel_evaluator:zip(Tokens)
    ).

wrap_expr_begin(Expr) ->
    [" eel_converter:to_string(fun()->", Expr].

wrap_expr_end(Expr) ->
    [Expr, " end)"].

wrap_expr(Expr) ->
    wrap_expr_end(wrap_expr_begin(Expr)).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% TODO: Improve tests

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
                   {2,{{1,12},"<h1>Title</h1>"}},
                   <<>>,<<>>,
                   {27,{{1,372},"<footer>Footer</footer>"}}],
                  [{1,{expr,{1,1},"foo"}},
                   {3,
                    {code,{1,26},"io:format(\"Print but not render me!~n\")"}},
                   [{4,{start_expr,{1,73},"case 1 of"}},
                    {5,{mid_expr,{1,89},"2 ->"}},
                    {6,{nested_expr,{1,99},{[{6,{{1,99},"<p>Foo</p>"}}],[]}}},
                    {7,{mid_expr,{1,109},"; Bar ->"}},
                    {8,
                     {nested_expr,
                      {1,123},
                      {[{8,{{1,123},"<p>"}},{24,{{1,336},"</p>"}}],
                       [[{9,{start_expr,{1,126},"case hello =:= world of"}},
                         {10,{mid_expr,{1,156},"true ->"}},
                         {11,{expr,{1,169},"hello"}},
                         {12,{mid_expr,{1,182},"; false ->"}},
                         {13,
                          {nested_expr,
                           {1,198},
                           {[{13,{{1,198},"<p>"}},<<>>,{22,{{1,322},"</p>"}}],
                            [{15,{code,{1,226},"ignore_me"}},
                             [{16,{start_expr,{1,243},"case car =:= bus of"}},
                              {17,{mid_expr,{1,269},"true ->"}},
                              {18,
                               {nested_expr,
                                {1,282},
                                {[{18,{{1,282},"Car"}}],[]}}},
                              {19,{mid_expr,{1,285},"; false ->"}},
                              {20,{expr,{1,301},"bus"}},
                              {21,{end_expr,{1,312},"end"}}]]}}},
                         {23,{end_expr,{1,326},"end"}}]]}}},
                    {25,{end_expr,{1,340},"end"}}],
                   {26,{expr,{1,350},"Foo = foo, Foo"}}]},
    {ok, {Result, _}} = eel_tokenizer:tokenize(Bin, #{engine => ?MODULE}),
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
    Expected =
        {[{1,{{1,1},<<"<h1>">>}},
                   {5,{{1,73},[<<"</h1>">>,<<"<ul>">>]}},
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
    Expected =
        ["<h1>",<<"EEl">>,
        ["</h1>","<ul>"],
        [[<<"<li>">>,<<"foo">>,<<"</li>">>],
         [<<"<li>">>,<<"bar">>,<<"</li>">>],
         [<<"<li>">>,<<"baz">>,<<"</li>">>]],
        "</ul>",
        [<<"<div>Item count: ">>,<<"3">>,<<"</div>">>,
         [<<"<ul>">>,
          [[<<"<li>">>,<<"1">>,<<"</li>">>],
           [<<"<li>">>,<<"2">>,<<"</li>">>],
           [<<"<li>">>,<<"3">>,<<"</li>">>]],
          <<"</ul>">>]]],
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
    Result = eel:eval(Bin, #{'List' => [foo, bar, baz]}),
    ?assertEqual(Expected, Result).

-endif.
