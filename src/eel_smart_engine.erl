-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    init/1,
    handle_expr/2,
    handle_text/2,
    handle_body/1
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(token(Name), fun
    ({_, Bin}) -> {Name, Bin};
    (Bin) ->      {Name, Bin}
end).
-define(text,        ?token(text)).
-define(expr,        ?token(expr)).
-define(start_expr,  ?token(start_expr)).
-define(mid_expr,    ?token(mid_expr)).
-define(end_expr,    ?token(end_expr)).
-define(debug,       ?token(debug)).
-define(start_debug, ?token(start_debug)).
-define(mid_debug,   ?token(mid_debug)).
-define(end_debug,   ?token(end_debug)).

%% Types
-type token_name() :: text
                      | expr
                      | start_expr
                      | mid_expr
                      | end_expr
                      | debug
                      | start_debug
                      | mid_debug
                      | end_debug.
-type token()   :: {token_name(), binary()}.
-type static()  :: list().
-type dynamic() :: list().

%% State
-record(state, {
    in     = text :: text | expr | {text | expr, debug},
    tokens = []   :: [token()]
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) ->
    {ok, #state{}}.

handle_expr({_Pos, {<<"=">>, <<".">>}, Expr}, State) ->
    {ok, push(?expr(Expr), State)};
handle_expr({_Pos, {<<"=">>, <<>>}, Expr}, State) ->
    {ok, push(?start_expr(Expr), State#state{in = expr})};
handle_expr({_Pos, {<<>>, <<>>}, Expr}, #state{in = expr} = State) ->
    {ok, push(?mid_expr(Expr), State)};
handle_expr({_Pos, {<<>>, <<".">>}, Expr}, #state{in = expr} = State) ->
    {ok, push(?end_expr(Expr), State)};
handle_expr({_Pos, {<<":">>, <<":">>}, Expr}, State) ->
    {ok, push(?debug(Expr), State)};
handle_expr({_Pos, {<<":">>, <<>>}, Expr}, #state{in = In} = State) ->
    {ok, push(?start_debug(Expr), State#state{in = {In, debug}})};
handle_expr({_Pos, {<<>>, <<>>}, Expr}, #state{in = {_, debug}} = State) ->
    {ok, push(?mid_debug(Expr), State)};
handle_expr({_Pos, {<<>>, <<":">>}, Expr}, #state{in = {In, debug}} = State) ->
    {ok, push(?end_debug(Expr), State#state{in = In})};
handle_expr({_Pos, {<<"%">>, <<>>}, _Expr}, State) ->
    {ok, State};
handle_expr(Token, State) ->
    {error, {unknown_marker, {Token, State}}}.

handle_text({_Pos, Text}, #state{in = In} = State) when In =:= text; In =:= expr ->
    {ok, push(?text(Text), State)}.

handle_body(#state{tokens = Tokens}) ->
    _SD = parse_sd(lists:reverse(Tokens)),
    % TODO: Continue parsing tokens to AST
    {ok, lists:reverse(Tokens)}.

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
    {[], SD} = parse_sd(Tokens, in_text, {[], []}),
    SD.

parse_sd([{expr, _} = H | T], In, {S, D}) ->
    parse_sd(T, In, {S, [H | D]});
parse_sd([{start_expr, _} = H | T], in_text, {S, D}) ->
    parse_sd(T, in_expr, {S, [H | D]});
parse_sd([{start_expr, _} | _] = T, in_expr, {S, D}) ->
    {Tokens, SD} = parse_sd(T, in_text, {[], []}),
    parse_sd(Tokens, in_expr, {S, [{nested_expr, SD} | D]});
parse_sd([{mid_expr, _} = H | T], in_expr, {S, D}) ->
    parse_sd(T, in_expr, {S, [H | D]});
parse_sd([{end_expr, _} = H | T], in_expr, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse([H | D])}};
parse_sd([{text, _} = H | T], in_text, {S, D}) ->
    parse_sd(T, in_text, {[H | S], D});
parse_sd([{text, _} = H | T], in_expr, {S, D}) ->
    parse_sd(T, in_expr, {S, [H | D]});
parse_sd([{debug, _} = H | T], Where, {S, D}) ->
    parse_sd(T, Where, {S, [H | D]});
parse_sd([{start_debug, _} = H | T], In, {S, D}) ->
    parse_sd(T, {In, in_debug}, {S, [H | D]});
parse_sd([{mid_debug, _} = H | T], {In, in_debug}, {S, D}) ->
    parse_sd(T, {In, in_debug}, {S, [H | D]});
parse_sd([{end_debug, _} = H | T], {In, in_debug}, {S, D}) ->
    parse_sd(T, In, {S, [H | D]});
parse_sd([], _, SD) ->
    {[], SD}.

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
                {ok, #state{tokens = [{expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, #state{in = expr, tokens = [{start_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<"=">>, <<>>}, {<<"<%= Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, #state{in = expr, tokens = [{mid_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<>>}, {<<"<% Foo %>">>, <<"Foo">>}}, #state{in = expr})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, #state{in = expr, tokens = [{end_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, #state{in = expr})
            )
        },
        {
            "Should return debug token",
            ?assertEqual(
                {ok, #state{tokens = [{debug, <<"io:format(Foo)">>}]}},
                handle_expr({{1, 1}, {<<":">>, <<":">>}, {<<"<%: io:format(Foo) :%>">>, <<"io:format(Foo)">>}}, #state{})
            )
        },
        {
            "Should return start_debug token",
            ?assertEqual(
                {ok, #state{in = {text, debug}, tokens = [{start_debug, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<":">>, <<>>}, {<<"<%= Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return mid_debug token",
            ?assertEqual(
                {ok, #state{in = {text, debug}, tokens = [{mid_debug, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<>>}, {<<"<% Foo %>">>, <<"Foo">>}}, #state{in = {text, debug}})
            )
        },
        {
            "Should return end_debug token",
            ?assertEqual(
                {ok, #state{in = text, tokens = [{end_debug, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<":">>}, {<<"<% Foo :%>">>, <<"Foo">>}}, #state{in = {text, debug}})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                {ok, #state{}},
                handle_expr({{1, 1}, {<<"%">>, <<>>}, {<<"<%% Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return unknown marker error",
            ?assertEqual(
                {error, {unknown_marker, {{{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, #state{}}}},
                handle_expr({{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, #state{})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                {ok, #state{tokens = [{text, <<"Foo">>}]}},
                handle_text({{1, 1}, <<"Foo">>}, #state{})
            )
        },
        {
            "Should throw error if text token when debugging",
            ?assertError(
                function_clause,
                handle_text({{1, 1}, <<"Foo">>}, #state{in = {text, debug}})
            )
        }
    ].

handle_body_test() ->
    Bin = <<
        "<h1>Title</h1>"
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %>"
            "<%: io:format(\"Print but not render me!~n\") :%>"
            "<p>"
                "<%= case hello =:= world of %>"
                "<% true -> %>"
                    "<%= hello .%>"
                "<% ; false -> %>"
                    "<p>"
                        "<%= case car =:= bus of %>"
                        "<% true -> %>"
                            "<%: One = 1, %>"
                            "<% Two = 1, %>"
                            "<% io:format(\"Sum: ~n\", [One + Two]) :%>"
                            "Car"
                        "<% ; false -> %>"
                            "<%= bus .%>"
                        "<% end .%>"
                    "</p>"
                "<% end .%>"
            "</p>"
        "<% end .%>"
    >>,
    Expected = [
        {text,<<"<h1>Title</h1>">>},
        {start_expr,<<"case 1 of">>},
        {mid_expr,<<"2 ->">>},
        {text,<<"<p>Foo</p>">>},
        {mid_expr,<<"; Bar ->">>},
        {debug,<<"io:format(\"Print but not render me!~n\")">>},
        {text,<<"<p>">>},
        {start_expr,<<"case hello =:= world of">>},
        {mid_expr,<<"true ->">>},
        {expr,<<"hello">>},
        {mid_expr,<<"; false ->">>},
        {text,<<"<p>">>},
        {start_expr,<<"case car =:= bus of">>},
        {mid_expr,<<"true ->">>},
        {start_debug,<<"One = 1,">>},
        {mid_debug,<<"Two = 1,">>},
        {end_debug,<<"io:format(\"Sum: ~n\", [One + Two])">>},
        {text,<<"Car">>},
        {mid_expr,<<"; false ->">>},
        {expr,<<"bus">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>}
    ],
    {ok, Tokens} = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    ?assertEqual(Expected, Tokens).

-endif.
