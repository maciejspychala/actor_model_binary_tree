-module(tree).
-export([loop/0]).

-record(node, {val=nil,
               left=nil,
               right=nil}).
-record(msg, {type,
              val}).

nd() -> nd(#node{}).
nd(Node) ->
    receive
        M = #msg{type=insert} ->
            New = insert(M, Node),
            io:format("~p\n", [New]),
            nd(New)
    end.

insert(M, N = #node{val=nil}) ->
    N#node{val=M#msg.val};
insert(M, N) when M#msg.val < N#node.val ->
    N#node{left=send_insert(M, N#node.left)};
insert(M, N) when M#msg.val >= N#node.val ->
    N#node{right=send_insert(M, N#node.right)}.

send_insert(M, nil) ->
    New = spawn(fun() -> nd() end),
    New!M,
    New;
send_insert(M, N) ->
    N!M,
    N.

loop() ->
    N = spawn(fun() -> nd() end),
    loop(N).
loop(Node) ->
    {ok, [V]} = io:fread("new value: ","~d"),
    case V of
        0 -> ok;
        V -> Node!#msg{type=insert, val=V},
             loop(Node)
    end.
