-define(unknown_marker_error(Token), begin
    {{Ln, Col}, {SMkr, EMkr}, Expr} = Token,
    error(unknown_marker, [{line, Ln},
                           {column, Col},
                           {expression, Expr},
                           {markers, {SMkr, EMkr}}])
end).
