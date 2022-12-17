-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    init/1,
    markers/0,
    handle_expr/4,
    handle_text/3,
    handle_body/1,
    compile/2
]).

%% Includes
-include("eel.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(token(Name), fun (Bin) -> {Name, Bin} end).
-define(text,        ?token(text)).
-define(expr,        ?token(expr)).
-define(start_expr,  ?token(start_expr)).
-define(mid_expr,    ?token(mid_expr)).
-define(end_expr,    ?token(end_expr)).
-define(comment,     ?token(comment)).
-define(debug,       ?token(debug)).
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
                      | comment
                      | debug.
-type token()   :: {token_name(), binary()}.
-type static()  :: list().
-type dynamic() :: list().

%% State
-record(state, {
    tokens = [] :: [token()]
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) -> #state{}.

markers() -> [{"<%=", ".%>"},
              {"<%=",  "%>"},
              {"<%",   "%>"},
              {"<%",  ".%>"},
              {"<%:", ":%>"},
              {"<%%", "%%>"}].

handle_expr(_Pos, {"<%=", ".%>"}, Expr, State) ->
    push(?expr(Expr), State);
handle_expr(_Pos, {"<%=", "%>"}, Expr, State) ->
    push(?start_expr(Expr), State);
handle_expr(_Pos, {"<%", "%>"}, Expr, State) ->
    push(?mid_expr(Expr), State);
handle_expr(_Pos, {"<%", ".%>"}, Expr, State) ->
    push(?end_expr(Expr), State);
handle_expr(_Pos, {"<%:", ":%>"}, Expr, State) ->
    push(?debug(Expr), State);
handle_expr(_Pos, {"<%%", "%%>"}, Expr, State) ->
    push(?comment(Expr), State);
handle_expr(Pos, Marker, Expr, _State) ->
    ?unknown_marker_error({Pos, Marker, Expr}).

handle_text(_Pos, Text, State) ->
    push(?text(Text), State).

handle_body(#state{tokens = Tokens}) ->
    lists:reverse(Tokens).

compile(Tokens, []) ->
    parse_sd(Tokens).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

push(Token, #state{tokens = Tokens} = State) ->
    State#state{tokens = [Token | Tokens]}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc Parses tokens to statics and dynamics.
%% @end
%% -----------------------------------------------------------------------------
-spec parse_sd(Tokens :: [token()]) -> {static(), dynamic()}.

parse_sd(Tokens) ->
    {[], SD} = do_parse_sd(Tokens),
    SD.

do_parse_sd(Tokens) ->
    do_parse_sd(Tokens, in_text, undefined, {[], []}).

do_parse_sd(Tokens, In, Prev, SD) ->
    case do_parse_sd_1(Tokens, In, Prev, SD) of
        {[], {S, D}} ->
            {[], {lists:reverse(S), lists:reverse(D)}};
        {RestTokens, Prev1, AccSD} ->
            do_parse_sd(RestTokens, in_text, Prev1, AccSD)
    end.

% Single line expression
do_parse_sd_1([{expr, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_sd_1([{expr, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Multiline expression
do_parse_sd_1([{start_expr, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_sd_2(T, H, in_expr, Prev, {S, [[H] | D]});
do_parse_sd_1([{start_expr, _} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
do_parse_sd_1([{mid_expr, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
do_parse_sd_1([{end_expr, _} = H | T], in_expr, _, {S, [HD | D]}) ->
    {T, H, {S, [lists:reverse([H | HD]) | D]}};
% Text
do_parse_sd_1([{text, Text} = H | T], in_text, Prev, {S, D}) ->
    do_parse_sd_2(T, H, in_text, Prev, {[Text | S], D});
do_parse_sd_1([{text, _} | _] = T, in_expr, Prev, SD) ->
    parse_nested_sd(T, Prev, SD);
% Comment
do_parse_sd_1([{comment, _} | T], In, Prev, {S, D}) ->
    do_parse_sd_1(T, In, Prev, {S, D});
% Debug
do_parse_sd_1([{debug, _} = H | T], in_text, Prev, {S, D}) ->
    do_parse_sd_2(T, H, in_text, Prev, {S, [H | D]});
do_parse_sd_1([{debug, _} = H | T], in_expr, Prev, {S, [HD | D]}) ->
    do_parse_sd_2(T, H, in_expr, Prev, {S, [[H | HD] | D]});
% Done
do_parse_sd_1([], _, _, SD) ->
    {[], SD}.

do_parse_sd_2(Tokens, Curr, In, Prev, {S, D}) ->
    SD = case should_push_empty_static(Prev, Curr, {S, D}) of
             true -> {[<<>> | S], D};
             false -> {S, D}
         end,
    do_parse_sd_1(Tokens, In, Curr, SD).

should_push_empty_static(_, Curr, {[], _}) when ?is_expr(Curr) -> true;
should_push_empty_static(Prev, Curr, {_, _}) -> ?is_expr(Prev) andalso
                                                ?is_expr(Curr).

% Nested
parse_nested_sd(T, Prev, {S, [HD | D]}) ->
    {Tokens, Nested} = parse_nested_sd_1(T, Prev, {[], []}),
    H = {nested_expr, Nested},
    do_parse_sd_1(Tokens, in_expr, H, {S, [[H | HD] | D]}).

parse_nested_sd_1([{start_expr, _} | _] = T, Prev, {S, D}) ->
    {Tokens, _, {S1, [H]}} = do_parse_sd_1(T, in_text, Prev, {S, []}),
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

% TODO: Retrieve funs vars
% retrieve_vars(Bin) ->
%     {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Bin)),
%     MaybeAcc =
%         fun(Var, VAcc) ->
%             case lists:member(Var, VAcc) of
%                 true -> VAcc;
%                 false -> [Var | VAcc]
%             end
%         end,
%     {_, Vars} =
%         lists:foldl(
%             fun
%                 ({var, _, Var} = Token, {[{':=', _} | _] = TAcc, VAcc}) ->
%                     {[Token | TAcc], MaybeAcc(Var, VAcc)};
%                 ({var, _, _} = Token, {[{'=>', _} | _] = TAcc, VAcc}) ->
%                     {[Token | TAcc], VAcc};
%                 ({var, _, Var} = Token, {TAcc, VAcc}) ->
%                     {[Token | TAcc], MaybeAcc(Var, VAcc)};
%                 (Token, {TAcc, VAcc}) ->
%                     {[Token | TAcc], VAcc}
%             end,
%             {[], []},
%             Tokens
%         ),
%     Vars.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return expr token",
            ?assertEqual(
                #state{tokens = [{expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%=", ".%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                #state{tokens = [{start_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%=", "%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                #state{tokens = [{mid_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%", "%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                #state{tokens = [{end_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%", ".%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return debug token",
            ?assertEqual(
                #state{tokens = [{debug, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%:", ":%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                #state{tokens = [{comment, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%%", "%%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should raise unknown marker error",
            ?assertError(
                unknown_marker,
                handle_expr({1, 1}, {"<%.", ".%>"}, <<"Foo">>, #state{})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                #state{tokens = [{text, <<"Foo">>}]},
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
    Expected = [
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
    Result = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    ?assertEqual(Expected, Result).

parse_sd_test() ->
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
    Result = parse_sd(Tokens),
    ?assertEqual(Expected, Result).

-endif.
