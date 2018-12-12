-module(tree).
-export([loop/0]).

-record(node, {val=nil,
               left=nil,
               right=nil}).

insert(V, N = #node{val=nil}) ->
    N#node{val=V};
insert(V, N) when V < N#node.val ->
    N#node{left=send_insert(V, N#node.left)};
insert(V, N) when V >= N#node.val ->
    N#node{right=send_insert(V, N#node.right)}.

send_insert(V, nil) ->
    New = spawn(fun() -> nd() end),
    New!{insert,V},
    New;
send_insert(V, N) ->
    N!{insert,V},
    N.

nd() -> nd(#node{}).
nd(Node) ->
    receive
        {insert, V} ->
            New = insert(V, Node),
            io:format("~p\n", [New]),
            nd(New)
    end.

loop() ->
    N = spawn(fun() -> nd() end),
    loop(N).
loop(Node) ->
    {ok, [V]} = io:fread("new value: ","~d"),
    case V of
        0 -> ok;
        V -> Node!{insert,V},
             loop(Node)
    end.
