-define(unknown_marker_error(Token), begin
    {{Ln, Col}, Marker, Expr} = Token,
    error(unknown_marker, [{line, Ln},
                           {column, Col},
                           {expression, Expr},
                           {marker, Marker}])
end).
