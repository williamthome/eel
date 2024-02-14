% @see https://www.geeksforgeeks.org/introduction-to-tree-data-structure-and-algorithm-tutorials/
-module(eel_tree).

-export([ new/0
        , new/1
        , add_vertex/1
        , add_vertex/2
        , add_vertex/3
        , add_edge/3

        , get_vertex_count/1
        , get_vertices/1
        , get_root/1
        , get_metadata/1
        , set_metadata/2

        , get_vertex_parent/1
        , get_vertex_label/1
        , get_vertex_children/1
        , get_vertex_is_root/1
        , get_vertex_metadata/1
        , set_vertex_metadata/2

        , fetch_vertex/2
        , fetch_vertex_parent/2
        , fetch_vertex_children/2

        , map_vertices/2
        , put_vertex/2
        ]).

-export_type([ tree/0
             , vertex/0
             , label/0
             , vertices/0
             , metadata/0
             ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record( tree,
       { vertex_count :: non_neg_integer()
       , vertices     :: #{label() := vertices()}
       , root         :: label()
       , metadata     :: metadata()
       }).

-record( vertex,
       { parent   :: label()
       , label    :: label()
       , children :: vertices()
       , is_root  :: boolean()
       , is_leaf  :: boolean()
       , metadata :: metadata()
       }).

-opaque tree()   :: #tree{}.
-opaque vertex() :: #vertex{}.

% TODO: Maybe rename to key.
-type label()    :: term().
-type vertices() :: [vertex()].
-type metadata() :: term().

%%======================================================================
%% API functions
%%======================================================================

new() ->
    new(#{}).

new(Opts) ->
    #tree{
        vertex_count = 0,
        vertices = [],
        root = undefined,
        metadata = maps:get(metadata, Opts, undefined)
    }.

add_vertex(Tree) ->
    add_vertex(Tree, #{}).

add_vertex(#vertex{} = Vertex, #tree{} = Tree0) ->
    Tree1 = put_vertex(Vertex, Tree0),
    Tree2 = incr_vertex_count(Tree1),
    Tree3 = maybe_set_vertex_as_root(Vertex, Tree2),
    case Vertex#vertex.parent of
        undefined ->
            {Vertex, Tree3};
        Parent ->
            VParent = fetch_vertex(Parent, Tree3),
            Tree = put_vertex(VParent#vertex{is_leaf = false}, Tree3),
            {Vertex, Tree}
    end;
add_vertex(Label, #tree{} = Tree) ->
    add_vertex(Label, Tree, #{});
add_vertex(#tree{} = Tree, Opts) ->
    Label = get_curr_index(Tree),
    add_vertex(Label, Tree, Opts).

add_vertex(Label, Tree, Opts) ->
    add_vertex(new_vertex(Label, Tree, Opts), Tree).

add_edge(#vertex{label = From}, To, Tree) ->
    add_edge(From, To, Tree);
add_edge(From, #vertex{label = To}, Tree) ->
    add_edge(From, To, Tree);
add_edge(FromLabel, ToLabel, Tree) ->
    From0 = fetch_vertex(FromLabel, Tree),
    To0 = fetch_vertex(ToLabel, Tree),
    From = From0#vertex{
        is_leaf = false,
        children = [To0#vertex.label | From0#vertex.children]
    },
    To = To0#vertex{parent = From0#vertex.label},
    put_vertices([From, To], Tree).

fetch_vertex(#vertex{label = Label}, Tree) ->
    fetch_vertex(Label, Tree);
fetch_vertex(Label, Tree) ->
    {Label, Vertex} = proplists:lookup(Label, get_vertices(Tree)),
    Vertex.

fetch_vertex_parent(Vertex, Tree) ->
    get_vertex_parent(fetch_vertex(Vertex, Tree)).

fetch_vertex_children(Vertex, Tree) ->
    lists:map( fun(Child) -> fetch_vertex(Child, Tree) end
             , get_vertex_children(Vertex) ).

get_metadata(#tree{metadata = Metadata}) -> Metadata.
set_metadata(Metadata, Tree) -> Tree#tree{metadata = Metadata}.

get_vertex_count(#tree{vertex_count = VertexCount}) -> VertexCount.
get_vertices(#tree{vertices = Vertices}) -> Vertices.
get_root(#tree{root = Root}) -> Root.

get_vertex_parent(#vertex{parent = Parent}) -> Parent.
get_vertex_label(#vertex{label = Label}) -> Label.
get_vertex_children(#vertex{children = Children}) -> Children.
get_vertex_is_root(#vertex{is_root = IsRoot}) -> IsRoot.
get_vertex_metadata(#vertex{metadata = Metadata}) -> Metadata.

set_vertex_metadata(Metadata, Vertex) ->
    Vertex#vertex{metadata = Metadata}.

map_vertices(Fun, Tree) when is_function(Fun, 2) ->
    Tree#tree{vertices = lists:map(Fun, get_vertices(Tree))}.

put_vertex(#vertex{label = Label} = Vertex, Tree) ->
    Vertices = get_vertices(Tree),
    Tree#tree{
        vertices = lists:keystore(Label, 1, Vertices, {Label, Vertex})
    }.

%%======================================================================
%% Internal functions
%%======================================================================

get_curr_index(#tree{vertex_count = Count}) ->
    Count.

incr_vertex_count(#tree{vertex_count = Count} = Tree) ->
    Tree#tree{vertex_count = Count + 1}.

% new_vertex(Label, Tree) ->
%     new_vertex(Label, Tree, #{}).

new_vertex(Label, Tree0, Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    Children = maps:get(children, Opts, []),
    IsRoot = maps:get(is_root, Opts, get_vertices(Tree0) =:= []),
    Metadata = maps:get(metadata, Opts, undefined),
    IsLeaf = maps:get(is_leaf, Opts, true),
    new_vertex(Parent, Label, Children, IsRoot, IsLeaf, Metadata).

new_vertex(Parent, Label, Children, IsRoot, IsLeaf, Metadata) ->
    #vertex{
        parent = Parent,
        label = Label,
        children = Children,
        is_root = IsRoot,
        is_leaf = IsLeaf,
        metadata = Metadata
    }.

set_root(#vertex{label = Label}, Tree) ->
    Tree#tree{root = Label}.

maybe_set_vertex_as_root(#vertex{is_root = true} = Vertex, Tree) ->
    set_root(Vertex, Tree);
maybe_set_vertex_as_root(#vertex{is_root = false}, Tree) ->
    Tree.

put_vertices(Vertices, Tree0) ->
    lists:foldl(fun put_vertex/2, Tree0, Vertices).

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

tree_test() ->
    Expected = {tree,3,
    [{root,{vertex,undefined,root,
                   [b,a],
                   true,false,undefined}},
     {a,{vertex,root,a,[],false,true,undefined}},
     {b,{vertex,root,b,[],false,true,undefined}}],
    root,undefined},
    Tree0 = new(),
    {VR, Tree1} = add_vertex(root, Tree0),
    {VA, Tree2} = add_vertex(a, Tree1),
    {VB, Tree3} = add_vertex(b, Tree2),
    Tree4 = add_edge(VR, VA, Tree3),
    Tree5 = add_edge(VR, VB, Tree4),
    Result = Tree5,
    ?assertEqual(Expected, Result).

-endif.
