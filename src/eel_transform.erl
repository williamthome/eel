-module(eel_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    lists:map(
        fun(Form) ->
            case erl_syntax:type(Form) =:= function andalso
                 erl_syntax:function_clauses(Form)
            of
                [{clause, Pos, Patterns, Guards, Body}] ->
                    LastBodyElem = lists:last(Body),
                    case erl_syntax:type(LastBodyElem) =:= tuple andalso 
                         erl_syntax:tuple_elements(LastBodyElem) 
                    of
                        [{atom, _, eel}, Defs] ->
                            [{atom, _, Action}, ActionArgs] = erl_syntax:tuple_elements(Defs),
                            EElAction = eel(Action, erl_syntax:tuple_elements(ActionArgs)),
                            NewBody = lists:droplast(Body) ++ [erl_syntax:revert(EElAction)],
                            NewForm = setelement(tuple_size(Form), Form, [
                                {clause, Pos, Patterns, Guards, NewBody}
                            ]),
                            NewForm;
                        _ ->
                            Form
                    end;
                _ ->
                    Form
            end
        end,
        Forms
    ).

eel(fun_from_binary, [{var, _, 'Bindings'}, {string, _, Text}, Opts]) ->
    eel_fun_from({binary, {Text, Opts}});
eel(fun_from_file, [{var, _, 'Bindings'}, {string, _, Filename}, Opts]) ->
    eel_fun_from({file, {Filename, Opts}}).

eel_fun_from({binary, {Text0, Opts0}}) ->
    Bin = flatten_ws(4, Text0),
    Opts = parserl_trans:eval(Opts0),
    Result = maps:get(result, Opts),
    do_eel_fun(Result, {binary, Bin, Opts});
eel_fun_from({file, {Filename, Opts0}}) ->
    Opts = parserl_trans:eval(Opts0),
    Result = maps:get(result, Opts),
    do_eel_fun(Result, {file, Filename, Opts}).

do_eel_fun(render, Defs) ->
    {ok, {Static, AST}} = compile(Defs),
    Vars = eel_compiler:ast_vars(AST),
    parserl_trans:quote(
        [ "eel_renderer:render( Bindings"
          "                   , #{ static => _@static",
          "                      , ast => _@ast",
          "                      , vars => _@vars } )" ]
        , #{ static => Static
           , ast => AST
           , vars => Vars } );
do_eel_fun(eval, Defs) ->
    {ok, {Static, AST}} = compile(Defs),
    Vars = eel_compiler:ast_vars(AST),
    parserl_trans:quote(
        [ "eel_evaluator:eval(",
          "    eel_renderer:render( Bindings"
          "                       , #{ static => _@static",
          "                          , ast => _@ast",
          "                          , vars => _@vars } ) )" ]
        , #{ static => Static
           , ast => AST
           , vars => Vars } ).

flatten_ws(N, Text) ->
    RE = lists:flatten(["\n", lists:duplicate(N, " ")]),
    re:replace(Text, RE, <<>>, [global, {return, binary}, unicode]).

compile({binary, Bin, Opts}) ->
    eel:compile(Bin, Opts);
compile({file, {priv, Filename}, Opts}) ->
    case application:get_application() of
        {ok, App} ->
            compile_priv_file(App, Filename, Opts);
        undefined ->
            error(undefined_application)
    end;
compile({file, {priv, App, Filename}, Opts})  ->
    compile_priv_file(App, Filename, Opts);
compile({file, Filename, Opts}) ->
    eel:compile_file(Filename, Opts).

compile_priv_file(App, Filename0, Opts) when is_atom(App),
                                             ( is_binary(Filename0) orelse
                                               is_list(Filename0) ) ->
    Filename = filename:join([code:priv_dir(App), Filename0]),
    eel:compile_file(Filename, Opts).
