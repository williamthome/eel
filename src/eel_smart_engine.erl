-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    handle_expr/2,
    handle_text/2,
    handle_end/1
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

handle_expr({{_Ln, _Col}, {<<"=">>, <<".">>}, {_ExpOut, <<>>}}, Acc) ->
    {ok, Acc};
handle_expr({{Ln, Col}, {<<"=">>, <<".">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"=">>, <<"">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {start_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"">>, <<"">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {mid_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"">>, <<".">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {end_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%">>, <<".">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {comment, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {StartMarker, EndMarker}, {ExpOut, ExpIn}}, Acc) ->
    {error, {unknown_marker, {{Ln, Col}, {StartMarker, EndMarker}, {ExpOut, ExpIn}, Acc}}}.

handle_text({_Cursor, Text}, [{{Ln, Col}, {text, TAcc}} | Acc]) ->
    {ok, [{{Ln, Col}, {text, <<TAcc/binary, Text/binary>>}} | Acc]};
handle_text({{Ln, Col}, Text}, Acc) ->
    {ok, [{{Ln, Col}, {text, Text}} | Acc]}.

handle_end(Tokens) ->
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
                {ok, []},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<>>, <<>>}}, [])
            )
        },
        {
            "Should return expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"=">>, <<".">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {start_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"=">>, <<"">>}, {<<"<%= Foo %>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {mid_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"">>, <<"">>}, {<<"<% Foo %>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {end_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"">>, <<".">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return comment token",
            ?assertEqual(
                {ok, [{{1, 1}, {comment, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%">>, <<".">>}, {<<"<%% Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return unknown marker error",
            ?assertEqual(
                {error, {unknown_marker, {{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}, []}}},
                handle_expr({{1, 1}, {<<".">>, <<".">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, [])
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                {ok, [{{1, 1}, {text, <<"Foo">>}}]},
                handle_text({{1, 1}, <<"Foo">>}, [])
            )
        },
        {
            "Should concatenate text",
            ?assertEqual(
                {ok, [{{1, 1}, {text, <<"FooBar">>}}]},
                handle_text(
                    {{1, 1}, <<"Bar">>},
                    [{{1, 1}, {text, <<"Foo">>}}]
                )
            )
        }
    ].

% TODO
handle_end_test() ->
    Bin = <<
        "<%= case 1 of %>"
        "<% 1 -> %><p>Foo</p>"
        "<% ; _ -> %><p>Bar</p>"
        "<% end .%>"
    >>,
    {ok, Tokens} = eel_tokenizer:tokenize(Bin, ?MODULE),
    % ?debugFmt("~p", [Tokens]),
    ok.

-endif.
