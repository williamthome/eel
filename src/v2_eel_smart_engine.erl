-module(v2_eel_smart_engine).

-export([markers/0]).

-include("v2_eel.hrl").

markers() ->
    [
        #marker{
            id = snapshot,
            start = <<"<%.">>,
            'end' = <<".%>">>,
            kind = snapshot
        },
        #marker{
            id = expr,
            start = <<"<%=">>,
            'end' = <<".%>">>
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            'end' = <<"%>">>,
            kind = multiplex
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            'end' = <<"%>">>,
            kind = multiplex
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            'end' = <<".%>">>
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            'end' = <<"%%>">>
        }
    ].
