-module(eel_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    case parserl:find_all_attributes(eel_fun, Forms) of
        [] ->
            logger:warning("No eel attribute found in ~p", [parserl:get_module(Forms)]),
            Forms;

        Attrs ->
            GlobalOpts = #{ if_fun_exists => append },
            parserl_trans:form(Forms, GlobalOpts, [
                parserl_trans:remove_attribute(eel_fun),
                parserl_trans:foreach(
                    fun(Attr) ->
                        Args = parserl:eval(erl_syntax:attribute_arguments(Attr)),
                        {FunName, Kind, FunOpts} = Args,
                        {ok, {Static, AST}} =
                            case Kind of
                                {bin, Bin, CompOpts} ->
                                    eel:compile(Bin, CompOpts);

                                {file, Filename, CompOpts} ->
                                    eel:compile_file(Filename, CompOpts)
                            end,
                        Vars = eel_compiler:ast_vars(AST),
                        Opts = #{ env => #{fun_name => FunName}
                                , export => maps:get(export, FunOpts, true) },
                        [
                            parserl_trans:insert_function(
                                "static('@fun_name') -> _@static.",
                                #{ env => #{ fun_name => FunName
                                           , static => Static } }),
                            parserl_trans:insert_function(
                                "ast('@fun_name') -> _@ast.",
                                #{ env => #{ fun_name => FunName
                                           , ast => AST } }),
                            parserl_trans:insert_function(
                                "vars('@fun_name') -> _@vars.",
                                #{ env => #{ fun_name => FunName
                                           , vars => Vars } }),
                            parserl_trans:if_else(
                                Vars =:= [],
                                [
                                    parserl_trans:if_else(
                                        maps:get(eval, FunOpts, false),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'() ->",
                                             "    eel_evaluator:eval(eel_renderer:render(",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    ))."], Opts),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'() ->",
                                             "    eel_renderer:render(",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    )."], Opts)
                                    )
                                ],
                                [
                                    parserl_trans:if_else(
                                        maps:get(eval, FunOpts, false),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'(Bindings) ->",
                                             "    eel_evaluator:eval(eel_renderer:render(",
                                             "        Bindings,",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    ))."], Opts),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'(Bindings) ->",
                                             "    eel_renderer:render(",
                                             "        Bindings,",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    )."], Opts)
                                    )
                                ]
                            )
                        ]
                    end,
                    Attrs
                ),

                parserl_trans:debug()
            ])
    end.
