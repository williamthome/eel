-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    init/1,
    handle_expr/3,
    handle_text/3,
    handle_body/2
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) -> [].

handle_expr({{_Ln, _Col}, {<<"=">>, <<".">>}, {_ExpOut, <<>>}}, Acc, State) ->
    {ok, {Acc, State}};
handle_expr({{Ln, Col}, {<<"=">>, <<".">>}, {_ExpOut, ExpIn}}, Acc, State) ->
    {ok, {[{{Ln, Col}, {expr, ExpIn}} | Acc], State}};
handle_expr({{Ln, Col}, {<<"=">>, <<"">>}, {_ExpOut, ExpIn}}, Acc, State) ->
    {ok, {[{{Ln, Col}, {start_expr, ExpIn}} | Acc], State}};
handle_expr({{Ln, Col}, {<<"">>, <<"">>}, {_ExpOut, ExpIn}}, Acc, State) ->
    {ok, {[{{Ln, Col}, {mid_expr, ExpIn}} | Acc], State}};
handle_expr({{Ln, Col}, {<<"">>, <<".">>}, {_ExpOut, ExpIn}}, Acc, State) ->
    {ok, {[{{Ln, Col}, {end_expr, ExpIn}} | Acc], State}};
handle_expr({{Ln, Col}, {<<"%">>, <<".">>}, {_ExpOut, ExpIn}}, Acc, State) ->
    {ok, {[{{Ln, Col}, {comment, ExpIn}} | Acc], State}};
handle_expr({{Ln, Col}, {StartMarker, EndMarker}, {ExpOut, ExpIn}}, Acc, _State) ->
    {error, {unknown_marker, {{Ln, Col}, {StartMarker, EndMarker}, {ExpOut, ExpIn}, Acc}}}.

handle_text({_Cursor, Text}, [{{Ln, Col}, {text, TAcc}} | Acc], State) ->
    {ok, {[{{Ln, Col}, {text, <<TAcc/binary, Text/binary>>}} | Acc], State}};
handle_text({{Ln, Col}, Text}, Acc, State) ->
    {ok, {[{{Ln, Col}, {text, Text}} | Acc], State}}.

handle_body(Tokens, _State) ->
    {ok, Tokens}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return empty if no expression",
            ?assertEqual(
                {ok, {[], []}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<>>, <<>>}}, [], [])
            )
        },
        {
            "Should return expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {expr, <<"Foo">>}}], []}},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, [], [])
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {start_expr, <<"Foo">>}}], []}},
                handle_expr({{1, 1}, {<<"=">>, <<"">>}, {<<"<%= Foo %>">>, <<"Foo">>}}, [], [])
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {mid_expr, <<"Foo">>}}], []}},
                handle_expr({{1, 1}, {<<"">>, <<"">>}, {<<"<% Foo %>">>, <<"Foo">>}}, [], [])
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, {[{{1, 1}, {end_expr, <<"Foo">>}}], []}},
                handle_expr({{1, 1}, {<<"">>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, [], [])
            )
        },
        {
            "Should return comment token",
            ?assertEqual(
                {ok, {[{{1, 1}, {comment, <<"Foo">>}}], []}},
                handle_expr({{1, 1}, {<<"%">>, <<".">>}, {<<"<%% Foo .%>">>, <<"Foo">>}}, [], [])
            )
        },
        {
            "Should return unknown marker error",
            ?assertEqual(
                {error, {unknown_marker, {{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}, []}}},
                handle_expr({{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, [], [])
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                {ok, {[{{1, 1}, {text, <<"Foo">>}}], []}},
                handle_text({{1, 1}, <<"Foo">>}, [], [])
            )
        },
        {
            "Should concatenate text",
            ?assertEqual(
                {ok, {[{{1, 1}, {text, <<"FooBar">>}}], []}},
                handle_text(
                    {{1, 1}, <<"Bar">>},
                    [{{1, 1}, {text, <<"Foo">>}}],
                    []
                )
            )
        }
    ].

% TODO
handle_body_test() ->
    Bin = <<
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %><p><%= Bar .%></p>"
        "<% end .%>"
    >>,
    eel_tokenizer:tokenize(Bin, ?MODULE, []),
    % {ok, Tokens} = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    % ?debugFmt("~p", [Tokens]),
    ok.

-endif.
