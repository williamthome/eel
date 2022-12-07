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
    (Index, {Ln, Col}, {_, Bin}) -> {Index, {Ln, Col}, {Name, Bin}, ?vars(Bin)};
    (Index, {Ln, Col}, Bin) -> {Index, {Ln, Col}, {Name, Bin}, []}
end).
-define(text, ?token(text)).
-define(expr, ?token(expr)).
-define(start, ?token(start_expr)).
-define(mid, ?token(mid_expr)).
-define('end', ?token(end_expr)).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) ->
    {ok, {[], 0, 0}}.

handle_expr({{_Ln, _Col}, {<<"=">>, <<".">>}, {_ExpOut, <<>>}}, State) ->
    {ok, State};
handle_expr({Pos, {<<"=">>, <<".">>}, Expr}, {Acc, EIdx, CIdx}) ->
    {ok, {[?expr({EIdx, CIdx + 1}, Pos, Expr) | Acc], EIdx, CIdx + 1}};
handle_expr({Pos, {<<"=">>, <<"">>}, Expr}, {Acc, EIdx, CIdx}) ->
    {ok, {[?start({EIdx + 1, CIdx + 1}, Pos, Expr) | Acc], EIdx + 1, CIdx + 1}};
handle_expr({Pos, {<<"">>, <<"">>}, Expr}, {Acc, EIdx, CIdx}) ->
    {ok, {[?mid({EIdx, CIdx + 1}, Pos, Expr) | Acc], EIdx, CIdx + 1}};
handle_expr({Pos, {<<"">>, <<".">>}, Expr}, {Acc, EIdx, CIdx}) ->
    {ok, {[?'end'({EIdx, CIdx + 1}, Pos, Expr) | Acc], EIdx - 1, CIdx + 1}};
handle_expr({{_Ln, _Col}, {<<"%">>, <<".">>}, {_ExpOut, _ExpIn}}, State) ->
    {ok, State};
handle_expr(Token, State) ->
    {error, {unknown_marker, {Token, State}}}.

handle_text({Pos, Text}, {Acc, EIdx, CIdx}) ->
    {ok, {[?text({EIdx, CIdx + 1}, Pos, Text) | Acc], EIdx, CIdx + 1}}.

handle_body({Tokens, _, _}) ->
    {ok, lists:reverse(Tokens)}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================



%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return empty if no expression",
            ?assertEqual(
                {ok, {[], 0, 0}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<>>, <<>>}}, {[], 0, 0})
            )
        },
        {
            "Should return expr token",
            ?assertEqual(
                {ok, {[{{0, 1}, {1, 1}, {expr, <<"Foo">>}, ['Foo']}], 0, 1}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, {[], 0, 0})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {1, 1}, {start_expr, <<"Foo">>}, ['Foo']}], 1, 1}},
                handle_expr({{1, 1}, {<<"=">>, <<"">>}, {<<"<%= Foo %>">>, <<"Foo">>}}, {[], 0, 0})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, {[{{0, 1}, {1, 1}, {mid_expr, <<"Foo">>}, ['Foo']}], 0, 1}},
                handle_expr({{1, 1}, {<<"">>, <<"">>}, {<<"<% Foo %>">>, <<"Foo">>}}, {[], 0, 0})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {1, 1}, {end_expr, <<"Foo">>}, ['Foo']}], 0, 1}},
                handle_expr({{1, 1}, {<<"">>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, {[], 1, 0})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                {ok, {[], 0, 0}},
                handle_expr({{1, 1}, {<<"%">>, <<".">>}, {<<"<%% Foo .%>">>, <<"Foo">>}}, {[], 0, 0})
            )
        },
        {
            "Should return unknown marker error",
            ?assertEqual(
                {error, {unknown_marker, {{{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, {[], 0, 0}}}},
                handle_expr({{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, {[], 0, 0})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                {ok, {[{{0, 1}, {1, 1}, {text, <<"Foo">>}, []}], 0, 1}},
                handle_text({{1, 1}, <<"Foo">>}, {[], 0, 0})
            )
        }
    ].

handle_body_test() ->
    Bin = <<
        "<h1>Title</h1>"
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %>"
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
        {{0, 1},{1,1},{text,<<"<h1>Title</h1>">>}, []},
        {{1, 2},{1,15},{start_expr,<<"case 1 of">>}, []},
        {{1, 3},{1,31},{mid_expr,<<"2 ->">>}, []},
        {{1, 4},{1,41},{text,<<"<p>Foo</p>">>}, []},
        {{1, 5},{1,51},{mid_expr,<<"; Bar ->">>}, ['Bar']},
        {{1, 6},{1,65},{text,<<"<p>">>}, []},
        {{2, 7},{1,68},{start_expr,<<"case hello =:= world of">>}, []},
        {{2, 8},{1,98},{mid_expr,<<"true ->">>}, []},
        {{2, 9},{1,111},{expr,<<"hello">>}, []},
        {{2, 10},{1,124},{mid_expr,<<"; false ->">>}, []},
        {{2, 11},{1,140},{text,<<"<p>">>}, []},
        {{3, 12},{1,143},{start_expr,<<"case car =:= bus of">>}, []},
        {{3, 13},{1,169},{mid_expr,<<"true ->">>}, []},
        {{3, 14},{1,182},{text,<<"Car">>}, []},
        {{3, 15},{1,185},{mid_expr,<<"; false ->">>}, []},
        {{3, 16},{1,201},{expr,<<"bus">>}, []},
        {{3, 17},{1,212},{end_expr,<<"end">>}, []},
        {{2, 18},{1,222},{text,<<"</p>">>}, []},
        {{2, 19},{1,226},{end_expr,<<"end">>}, []},
        {{1, 20},{1,236},{text,<<"</p>">>}, []},
        {{1, 21},{1,240},{end_expr,<<"end">>}, []}
    ],
    {ok, Tokens} = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    ?assertEqual(Expected, Tokens).

-endif.
