-module(tree).
-export([nd/1, insert/2]).

-record(node, {val=nil,
               left=nil,
               right=nil}).

nd(V) ->
    #node{val=V}.

insert(V, nil) -> nd(V);
insert(V, N = #node{val=nil}) -> N#node{val=V};
insert(V, N) when V < N#node.val -> N#node{left=insert(V, N#node.left)};
insert(V, N) when V >= N#node.val -> N#node{right=insert(V, N#node.right)}.
