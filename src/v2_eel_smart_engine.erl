-module(v2_eel_smart_engine).

-export([markers/0, handle_tokens/1]).

-include("v2_eel.hrl").

markers() ->
    [
        #marker{
            id = snapshot,
            start = <<"<%.">>,
            final = <<".%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr,
            start = <<"<%=">>,
            final = <<".%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            final = <<"%>">>,
            tree_behavior = open
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            final = <<"%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            final = <<".%>">>,
            tree_behavior = close
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            final = <<"%%>">>,
            tree_behavior = ignore
        }
    ].

handle_tokens(Tokens) ->
    Tokens.
