-module(eel_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    deepmap(
        fun
            ({call, _, {remote, _, {atom, _, eel}, {atom, _, compile}}, _} = Form0) ->
                {value, Form, []} = erl_eval:expr(Form0, []),
                erl_syntax:revert(erl_syntax:abstract(Form));
            (Form) ->
                Form
        end,
        Forms
    ).

deepmap(Fun, Forms) when is_function(Fun, 1), is_list(Forms) ->
    do_deepmap(Forms, Fun).

do_deepmap({match, Pos, A, B}, F) ->
    F({match, Pos, do_deepmap(A, F), do_deepmap(B, F)});
do_deepmap({call, Pos, Fun, Args}, F) ->
    F({call, Pos, Fun, do_deepmap(Args, F)});
do_deepmap({clause, CPos, CPattern, CGuards, CBody}, F) ->
    F({clause, CPos, do_deepmap(CPattern, F), do_deepmap(CGuards, F), do_deepmap(CBody, F)});
do_deepmap({function, Pos, Name, Arity, Clauses}, F) ->
    F({function, Pos, Name, Arity, do_deepmap(Clauses, F)});
do_deepmap({'case', Pos, Cond, Clauses}, F) ->
    F({'case', Pos, do_deepmap(Cond, F), do_deepmap(Clauses, F)});
do_deepmap({'if', Pos, Clauses}, F) ->
    F({'if', Pos, do_deepmap(Clauses, F)});
do_deepmap(Forms, F) when is_list(Forms) ->
    lists:map(fun(Form) -> do_deepmap(Form, F) end, Forms);
do_deepmap(Form, F) ->
    F(Form).
