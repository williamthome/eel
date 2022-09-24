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
-spec to_module(What, filename()) -> {ok, module()} | {error, term()} when
    What ::
        {binary, binary()}
        | file
        | {priv_file, app()}
        | eel_compile:return().

to_module({binary, Bin}, FileName) ->
    Compiled = eel_compile:binary(Bin),
    to_module(Compiled, FileName);
to_module(file, FileName) ->
    Compiled = eel_compile:file(FileName),
    to_module(Compiled, FileName);
to_module({priv_file, App}, FileName) ->
    Compiled = eel_compile:priv_file(App, FileName),
    to_module(Compiled, FileName);
to_module({Static, AST}, FileName) ->
    Basename = filename:basename(FileName),
    ModuleBin = binary:replace(Basename, <<".">>, <<"_">>, [global]),
    Module =
        case catch erlang:binary_to_existing_atom(ModuleBin) of
            Atom when is_atom(Atom) -> Atom;
            _ -> erlang:binary_to_atom(ModuleBin)
        end,
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            Forms = merl:quote([
                "-module('@Module@').",
                "",
                "-export([",
                "   filename/0,",
                "   static/0,",
                "   ast/0,",
                "   render/1,",
                "   render/2",
                "]).",
                "",
                "filename() -> _@FileName@.",
                "",
                "static() -> _@Static@.",
                "",
                "ast() -> _@AST@.",
                "",
                "render(Bindings) ->",
                "   render(#{}, Bindings).",
                "",
                "render(Memo, Bindings) ->",
                "   eel_render:compiled({_@Static@, _@AST@}, Memo, Bindings)."
            ]),
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

% nothing here yet!