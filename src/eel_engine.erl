%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl engine behaviour.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_engine).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type index()         :: pos_integer().
-type line()          :: pos_integer().
-type column()        :: pos_integer().
-type position()      :: {line(), column()}.
-type expression()    :: binary().
-type state()         :: term().
-type token()         :: {index(), {position(), expression()}}.
-type static()        :: [token()].
-type dynamic()       :: [token()].
-type ast()           :: erl_syntax:syntaxTree().
-type marker_id()     :: term().
-type marker_symbol() :: binary().
-type marker_size()   :: non_neg_integer().
-type marker()        :: {marker_id(), { StartMarker :: marker_symbol()
                                       , EndMarker   :: marker_symbol() }}
                                       .
-type marker_token()  :: {index(), {marker_id(), position(), expression()}}.
-type expressions()   :: {Outer :: expression(), Inner :: expression()}.
-type options()       :: map().

-export_type([ index/0
             , line/0
             , column/0
             , expression/0
             , state/0
             , token/0
             , static/0
             , dynamic/0
             , ast/0
             , position/0
             , marker_id/0
             , marker_symbol/0
             , marker/0
             , expressions/0
             , options/0
             ]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback init(Opts) -> Result
    when Opts   :: options()
       , Result :: {ok, state()} | {error, term()}
       .

%% tokenize
-callback handle_expr_start(Bin, State) -> Result
    when Bin             :: binary()
       , State           :: state()
       , Result          :: {ok, Data} | {error, term()}
       , Data            :: {StartMarker, StartMarkerSize, Rest, NewState}
                          | {ok, NewState}
       , StartMarker     :: marker_symbol()
       , StartMarkerSize :: marker_size()
       , Rest            :: binary()
       , NewState        :: state()
       .

-callback handle_expr_end(StartMarker, Bin, State) -> Result
    when StartMarker   :: marker_symbol()
       , Bin           :: binary()
       , State         :: state()
       , Result        :: {ok, Data} | {error, term()}
       , Data          :: {MarkerId, EndMarkerSize, Rest, NewState}
                        | {ok, NewState}
       , MarkerId      :: marker_id()
       , EndMarkerSize :: marker_size()
       , Rest          :: binary()
       , NewState      :: state()
       .

-callback handle_text(Text, Pos, State) -> Result
    when Text     :: nonempty_string()
       , Pos      :: position()
       , State    :: state()
       , Result   :: {ok, Data} | {error, term()}
       , Data     :: {NewText, NewPos, NewState}
       , NewText  :: nonempty_string()
       , NewPos   :: position()
       , NewState :: state()
       .

-callback handle_body(Tokens, State) -> Result
    when Tokens :: [token()]
       , State  :: state()
       , Result :: {ok, {static(), dynamic()}} | {error, term()}
       .

%% compile
-callback handle_compile(MarkerToken, State) -> Result
    when MarkerToken :: marker_token()
       , State       :: state()
       , Result      :: {ok, {Token, NewState}} | {error, term()}
       , Token       :: token()
       , NewState    :: state()
       .

-callback handle_ast(AST, State) -> Result
    when AST    :: ast()
       , State  :: state()
       , Result :: {ok, NewAST} | {error, term()}
       , NewAST :: ast()
       .
