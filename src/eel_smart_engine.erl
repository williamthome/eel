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
    (Bin) -> {Name, Bin}
end).
-define(text, ?token(text)).
-define(expr, ?token(expr)).
-define(start, ?token(start_expr)).
-define(mid, ?token(mid_expr)).
-define('end', ?token(end_expr)).
-define(echo, ?token(echo)).

%% State
-record(state, {
    tokens = [] :: list()
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) ->
    {ok, #state{}}.

handle_expr({_Pos, {<<"=">>, <<".">>}, {_ExpOut, <<>>}}, State) ->
    {ok, State};
handle_expr({_Pos, {<<"=">>, <<".">>}, Expr}, State) ->
    {ok, push(?expr(Expr), State)};
handle_expr({_Pos, {<<"=">>, <<>>}, Expr}, State) ->
    {ok, push(?start(Expr), State)};
handle_expr({_Pos, {<<>>, <<>>}, Expr}, State) ->
    {ok, push(?mid(Expr), State)};
handle_expr({_Pos, {<<>>, <<".">>}, Expr}, State) ->
    {ok, push(?'end'(Expr), State)};
handle_expr({_Pos, {<<":">>, <<".">>}, Expr}, State) ->
    {ok, push(?echo(Expr), State)};
handle_expr({_Pos, {<<"%">>, <<>>}, _Expr}, State) ->
    {ok, State};
handle_expr(Token, State) ->
    {error, {unknown_marker, {Token, State}}}.

handle_text({_Pos, Text}, State) ->
    {ok, push(?text(Text), State)}.

handle_body(#state{tokens = Tokens}) ->
    {ok, lists:reverse(Tokens)}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

push(Token, #state{tokens = Tokens}) ->
    #state{tokens = [Token | Tokens]}.

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
            "Should return empty if no expression",
            ?assertEqual(
                {ok, #state{}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<>>, <<>>}}, #state{})
            )
        },
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
                {ok, #state{tokens = [{start_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<"=">>, <<>>}, {<<"<%= Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, #state{tokens = [{mid_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<>>}, {<<"<% Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, #state{tokens = [{end_expr, <<"Foo">>}]}},
                handle_expr({{1, 1}, {<<>>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return echo token",
            ?assertEqual(
                {ok, #state{tokens = [{echo, <<"io:format(Foo)">>}]}},
                handle_expr({{1, 1}, {<<":">>, <<".">>}, {<<"<%: io:format(Foo) .%>">>, <<"io:format(Foo)">>}}, #state{})
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
        }
    ].

handle_body_test() ->
    Bin = <<
        "<h1>Title</h1>"
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %>"
            "<%: io:format(\"Print but not render me!~n\") .%>"
            "<p>"
                "<%= case hello =:= world of %>"
                "<% true -> %>"
                    "<%= hello .%>"
                "<% ; false -> %>"
                    "<p>"
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
    >>,
    Expected = [
        {text,<<"<h1>Title</h1>">>},
        {start_expr,<<"case 1 of">>},
        {mid_expr,<<"2 ->">>},
        {text,<<"<p>Foo</p>">>},
        {mid_expr,<<"; Bar ->">>},
        {echo,<<"io:format(\"Print but not render me!~n\")">>},
        {text,<<"<p>">>},
        {start_expr,<<"case hello =:= world of">>},
        {mid_expr,<<"true ->">>},
        {expr,<<"hello">>},
        {mid_expr,<<"; false ->">>},
        {text,<<"<p>">>},
        {start_expr,<<"case car =:= bus of">>},
        {mid_expr,<<"true ->">>},
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
