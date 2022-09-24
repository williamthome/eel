%%%---------------------------------------------------------------------------------------
%%% @doc EEl output module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%---------------------------------------------------------------------------------------
-module(eel_output).

-export([
    to_module/2
]).

-include_lib("syntax_tools/include/merl.hrl").

-type app() :: atom().
-type filename() :: binary().

%%------------------------------------------------------------------------------
%% @doc Compiles code to a module.
%% @end
%%------------------------------------------------------------------------------
-spec to_module(What, filename() | module()) -> {ok, module()} | {error, term()} when
    What ::
        {binary, binary()}
        | file
        | {priv_file, app()}
        | eel_compile:return().

to_module({binary, Bin}, ModName) ->
    Compiled = eel_compile:binary(Bin),
    to_module(Compiled, ModName);
to_module(file, FileName) ->
    Compiled = eel_compile:file(FileName),
    to_module(Compiled, FileName);
to_module({priv_file, App}, FileName) ->
    Compiled = eel_compile:priv_file(App, FileName),
    to_module(Compiled, FileName);
to_module({Static, AST}, FileOrModName) ->
    {ModuleBin, Module} = module_name(FileOrModName),
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            Forms = merl:qquote(
                [
                    "-module('@Module@').",
                    "",
                    "-export([",
                    "   filename/0,",
                    "   static/0,",
                    "   ast/0,",
                    "   render/1,",
                    "   render/2,",
                    "   eval/1,",
                    "   eval/2",
                    "]).",
                    "",
                    "filename() -> _@filename.",
                    "",
                    "static() -> _@Static@.",
                    "",
                    "ast() -> _@AST@.",
                    "",
                    "render(Bindings) ->",
                    "   render(#{}, Bindings).",
                    "",
                    "render(Memo, Bindings) ->",
                    "   eel_render:compiled({_@Static@, _@AST@}, Memo, Bindings).",
                    "",
                    "eval(Bindings) ->",
                    "   eval(#{}, Bindings).",
                    "",
                    "eval(Memo, Bindings) ->",
                    "   eel_eval:compiled({_@Static@, _@AST@}, Memo, Bindings)."
                ],
                [
                    {filename,
                        merl:term(
                            case is_binary(FileOrModName) of
                                true -> FileOrModName;
                                false -> undefined
                            end
                        )}
                ]
            ),
            Forms1 = [erl_syntax:revert(Form) || Form <- Forms],
            case compile:forms(Forms1) of
                {ok, Module, Bin} ->
                    ModuleFileName = erlang:binary_to_list(<<ModuleBin/binary, ".erl">>),
                    case code:load_binary(Module, ModuleFileName, Bin) of
                        {module, Module} ->
                            {ok, Module};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    {error, {compile, Module}}
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

module_name(FileName) when is_binary(FileName) ->
    Basename = filename:basename(FileName),
    ModName = binary:replace(Basename, <<".">>, <<"_">>, [global]),
    Mod =
        case catch erlang:binary_to_existing_atom(ModName) of
            Atom when is_atom(Atom) -> Atom;
            _ -> erlang:binary_to_atom(ModName)
        end,
    {ModName, Mod};
module_name(ModName) when is_atom(ModName) ->
    Mod = erlang:atom_to_binary(ModName),
    {ModName, Mod}.
