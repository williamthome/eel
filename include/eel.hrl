-define(unknown_marker_error(Token), begin
    {{Ln, Col}, {_SMkr, _EMkr}, {Expr, _}} = Token,
    error(unknown_marker, [{line, Ln}, {column, Col}, {expression, Expr}])
end).
