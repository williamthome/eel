-module(eel_tree).

-export([ new/0
        , add_vertex/1
        , add_vertex/2
        , add_vertex/3
        , add_edge/3
        ]).

-export_type([ tree/0
             , vertex/0
             ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(tree, { vertex_count :: non_neg_integer()
              , vertices     :: #{label := [vertex()]}
              , root      :: label()
              }).

-record(vertex, { parent   :: label()
                , label    :: label()
                , children :: [vertex()]
                , is_root  :: boolean()
                }).

-opaque tree()   :: #tree{}.
-opaque vertex() :: #vertex{}.

-type label() :: term().

%%%=============================================================================
%%% API functions
%%%=============================================================================

new() ->
    #tree{
        vertex_count = 0,
        vertices = #{},
        root = undefined
    }.

add_vertex(Tree) ->
    Label = get_curr_index(Tree),
    add_vertex(Tree, Label).

add_vertex(#vertex{} = Vertex, Tree0) ->
    Tree1 = put_vertex(Vertex, Tree0),
    Tree2 = incr_vertex_count(Tree1),
    Tree = maybe_set_vertex_as_root(Vertex, Tree2),
    {Vertex, Tree};
add_vertex(Label, Tree) ->
    add_vertex(Label, Tree, #{}).

add_vertex(Label, Tree, Opts) ->
    add_vertex(new_vertex(Label, Tree, Opts), Tree).

add_edge(#vertex{label = From}, To, Tree) ->
    add_edge(From, To, Tree);
add_edge(From, #vertex{label = To}, Tree) ->
    add_edge(From, To, Tree);
add_edge(FromLabel, ToLabel, Tree) ->
    From0 = get_vertex(FromLabel, Tree),
    To0 = get_vertex(ToLabel, Tree),
    From = From0#vertex{
        children = [To0#vertex.label | From0#vertex.children]
    },
    To = To0#vertex{parent = From0#vertex.label},
    put_vertices([From, To], Tree).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_curr_index(#tree{vertex_count = Count}) ->
    Count.

incr_vertex_count(#tree{vertex_count = Count} = Tree) ->
    Tree#tree{vertex_count = Count + 1}.

% new_vertex(Label, Tree) ->
%     new_vertex(Label, Tree, #{}).

new_vertex(Label, Tree, Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    Children = maps:get(children, Opts, []),
    IsRoot = maps:get(is_root, Opts, map_size(get_vertices(Tree)) =:= 0),
    new_vertex(Parent, Label, Children, IsRoot).

new_vertex(Parent, Label, Children, IsRoot) ->
    #vertex{
        parent = Parent,
        label = Label,
        children = Children,
        is_root = IsRoot
    }.

set_root(#vertex{label = Label}, Tree) ->
    Tree#tree{root = Label}.

maybe_set_vertex_as_root(#vertex{is_root = true} = Vertex, Tree) ->
    set_root(Vertex, Tree);
maybe_set_vertex_as_root(#vertex{is_root = false}, Tree) ->
    Tree.

get_vertex(Label, Tree) ->
    maps:get(Label, get_vertices(Tree)).

get_vertices(#tree{vertices = Vertices}) ->
    Vertices.

put_vertex(#vertex{label = Label} = Vertex, Tree) ->
    Vertices = get_vertices(Tree),
    Tree#tree{
        vertices = Vertices#{
            Label => Vertex
        }
    }.

put_vertices(Vertices, Tree0) ->
    lists:foldl(fun put_vertex/2, Tree0, Vertices).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tree_test() ->
    Tree0 = new(),
    {VR, Tree1} = add_vertex(root, Tree0),
    {VA, Tree2} = add_vertex(a, Tree1),
    {VB, Tree3} = add_vertex(b, Tree2),
    Tree4 = add_edge(VR, VA, Tree3),
    Tree5 = add_edge(VR, VB, Tree4),
    ?debugFmt("~p", [Tree5]),
    ok.

-endif.
