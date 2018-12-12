-module(tree_simple).
-export([nd/1, insert/2, loop/0]).

-record(node, {val=nil,
               left=nil,
               right=nil}).

nd(V) ->
    #node{val=V}.

insert(V, nil) -> nd(V);
insert(V, N = #node{val=nil}) -> N#node{val=V};
insert(V, N) when V < N#node.val -> N#node{left=insert(V, N#node.left)};
insert(V, N) when V >= N#node.val -> N#node{right=insert(V, N#node.right)}.

loop() -> loop(nd(nil)).
loop(Node) ->
    io:format("~p\n", [Node]),
    {ok, [V]} = io:fread("new value: ","~d"),
    case V of
        0 -> ok;
        V -> New = insert(V, Node),
             loop(New)
    end.
