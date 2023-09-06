-module(v2_eel_smart_engine).

-export([markers/0]).

-include("v2_eel.hrl").

markers() ->
    [
        #marker{
            id = snapshot,
            start = <<"<%.">>,
            'end' = <<".%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr,
            start = <<"<%=">>,
            'end' = <<".%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            'end' = <<"%>">>,
            tree_behavior = open
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            'end' = <<"%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            'end' = <<".%>">>,
            tree_behavior = close
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            'end' = <<"%%>">>,
            tree_behavior = ignore
        }
    ].
