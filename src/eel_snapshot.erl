-module(eel_snapshot).

-export([ new/2
        , new/3
        , new/4
        , new/6
        , get_static/1
        , get_dynamic/1
        , get_ast/1
        , get_bindings/1
        , get_vars/1
        , get_changes/1
        ]).

-export_type([ snapshot/0
             , bindings/0
             , static/0
             , dynamic/0
             , ast/0
             , vars/0
             , changes/0
             ]).

-record(snapshot, { static   :: static()
                  , dynamic  :: dynamic()
                  , ast      :: ast()
                  , bindings :: bindings()
                  , vars     :: vars()
                  , changes  :: changes()
                  }).

%% Types
-type snapshot() :: #snapshot{}.
-type bindings() :: map().
-type static()   :: eel_engine:static().
-type dynamic()  :: undefined | [binary()].
-type ast()      :: eel_engine:ast().
-type vars()     :: [atom()].
-type changes()  :: [{non_neg_integer(), binary()}].

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc new/2.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, AST) -> Result
    when Static :: static()
       , AST    :: ast()
       , Result :: snapshot()
       .

new(Static, AST) ->
    new(Static, undefined, AST).

%% -----------------------------------------------------------------------------
%% @doc new/3.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST) -> Result
    when Static  :: static()
       , Dynamic :: dynamic()
       , AST     :: ast()
       , Result  :: snapshot()
       .

new(Static, Dynamic, AST) ->
    new(Static, Dynamic, AST, #{}).

%% -----------------------------------------------------------------------------
%% @doc new/4.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST, Bindings) -> Result
    when Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , Result   :: snapshot()
       .

new(Static, Dynamic, AST, Bindings) ->
    Vars = eel_compiler:ast_vars(AST),
    new(Static, Dynamic, AST, Bindings, Vars, []).

%% -----------------------------------------------------------------------------
%% @doc new/5.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST, Bindings, Vars, Changes) -> Result
    when Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , Vars     :: [atom()]
       , Changes  :: changes()
       , Result   :: snapshot()
       .

new(Static, Dynamic, AST, Bindings, Vars, Changes) ->
    #snapshot{ static   = Static
             , dynamic  = Dynamic
             , ast      = AST
             , bindings = Bindings
             , vars     = Vars
             , changes  = Changes
             }.

get_static(#snapshot{static = Static}) ->
    Static.

get_dynamic(#snapshot{dynamic = Dynamic}) ->
    Dynamic.

get_ast(#snapshot{ast = AST}) ->
    AST.

get_bindings(#snapshot{bindings = Bindings}) ->
    Bindings.

get_vars(#snapshot{vars = Vars}) ->
    Vars.

get_changes(#snapshot{changes = Changes}) ->
    Changes.
