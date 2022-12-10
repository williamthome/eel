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
-define(vars(Bin), begin
    {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Bin)),
    MaybeAcc =
        fun(Var, VAcc) ->
            case lists:member(Var, VAcc) of
                true -> VAcc;
                false -> [Var | VAcc]
            end
        end,
    {_, Vars} =
        lists:foldl(
            fun
                ({var, _, Var} = Token, {[{':=', _} | _] = TAcc, VAcc}) ->
                    {[Token | TAcc], MaybeAcc(Var, VAcc)};
                ({var, _, _} = Token, {[{'=>', _} | _] = TAcc, VAcc}) ->
                    {[Token | TAcc], VAcc};
                ({var, _, Var} = Token, {TAcc, VAcc}) ->
                    {[Token | TAcc], MaybeAcc(Var, VAcc)};
                (Token, {TAcc, VAcc}) ->
                    {[Token | TAcc], VAcc}
            end,
            {[], []},
            Tokens
        ),
    Vars
end).
-define(token(Name), fun
    ({Ln, Col}, {_, Bin}) -> {{Ln, Col}, {Name, Bin}, ?vars(Bin)};
    ({Ln, Col}, Bin) -> {{Ln, Col}, {Name, Bin}, []}
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

handle_expr({{_Ln, _Col}, {<<"=">>, <<".">>}, {_ExpOut, <<>>}}, State) ->
    {ok, State};
handle_expr({Pos, {<<"=">>, <<".">>}, Expr}, State) ->
    {ok, push(?expr(Pos, Expr), State)};
handle_expr({Pos, {<<"=">>, <<"">>}, Expr}, State) ->
    {ok, push(?start(Pos, Expr), State)};
handle_expr({Pos, {<<"">>, <<"">>}, Expr}, State) ->
    {ok, push(?mid(Pos, Expr), State)};
handle_expr({Pos, {<<"">>, <<".">>}, Expr}, State) ->
    {ok, push(?'end'(Pos, Expr), State)};
handle_expr({Pos, {<<":">>, <<".">>}, Expr}, State) ->
    {ok, push(?echo(Pos, Expr), State)};
handle_expr({{_Ln, _Col}, {<<"%">>, <<>>}, {_ExpOut, _ExpIn}}, State) ->
    {ok, State};
handle_expr(Token, State) ->
    {error, {unknown_marker, {Token, State}}}.

handle_text({Pos, Text}, State) ->
    {ok, push(?text(Pos, Text), State)}.

handle_body(#state{tokens = Tokens}) ->
    {ok, lists:reverse(Tokens)}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

push(Token, #state{tokens = Tokens}) ->
    #state{tokens = [Token | Tokens]}.

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
                {ok, #state{tokens = [{{1, 1}, {expr, <<"Foo">>}, ['Foo']}]}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, #state{tokens = [{{1, 1}, {start_expr, <<"Foo">>}, ['Foo']}]}},
                handle_expr({{1, 1}, {<<"=">>, <<"">>}, {<<"<%= Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, #state{tokens = [{{1, 1}, {mid_expr, <<"Foo">>}, ['Foo']}]}},
                handle_expr({{1, 1}, {<<"">>, <<"">>}, {<<"<% Foo %>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, #state{tokens = [{{1, 1}, {end_expr, <<"Foo">>}, ['Foo']}]}},
                handle_expr({{1, 1}, {<<"">>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, #state{})
            )
        },
        {
            "Should return echo token",
            ?assertEqual(
                {ok, #state{tokens = [{{1, 1}, {echo, <<"io:format(Foo)">>}, ['Foo']}]}},
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
                {ok, #state{tokens = [{{1, 1}, {text, <<"Foo">>}, []}]}},
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
    Expected =  [
        {{1,1},{text,<<"<h1>Title</h1>">>},[]},
        {{1,15},{start_expr,<<"case 1 of">>},[]},
        {{1,31},{mid_expr,<<"2 ->">>},[]},
        {{1,41},{text,<<"<p>Foo</p>">>},[]},
        {{1,51},{mid_expr,<<"; Bar ->">>},['Bar']},
        {{1,65},{echo,<<"io:format(\"Print but not render me!~n\")">>},[]},
        {{1,112},{text,<<"<p>">>},[]},
        {{1,115},{start_expr,<<"case hello =:= world of">>},[]},
        {{1,145},{mid_expr,<<"true ->">>},[]},
        {{1,158},{expr,<<"hello">>},[]},
        {{1,171},{mid_expr,<<"; false ->">>},[]},
        {{1,187},{text,<<"<p>">>},[]},
        {{1,190},{start_expr,<<"case car =:= bus of">>},[]},
        {{1,216},{mid_expr,<<"true ->">>},[]},
        {{1,229},{text,<<"Car">>},[]},
        {{1,232},{mid_expr,<<"; false ->">>},[]},
        {{1,248},{expr,<<"bus">>},[]},
        {{1,259},{end_expr,<<"end">>},[]},
        {{1,269},{text,<<"</p>">>},[]},
        {{1,273},{end_expr,<<"end">>},[]},
        {{1,283},{text,<<"</p>">>},[]},
        {{1,287},{end_expr,<<"end">>},[]}
    ],
    {ok, Tokens} = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    ?assertEqual(Expected, Tokens).

-endif.
