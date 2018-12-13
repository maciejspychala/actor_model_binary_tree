-module(tree).
-export([loop/0]).

-record(node, {val=nil,
               left=nil,
               right=nil}).
-record(msg, {type,
              val,
              client}).

nd() -> nd(#node{}).
nd(Node) ->
    receive
        M = #msg{type=insert} ->
            New = insert(M, Node),
            nd(New)
    end.

insert(M, N = #node{val=nil}) ->
    New = N#node{val=M#msg.val},
    M#msg.client!M#msg{type=inserted},
    New;
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

client() ->
    receive
        M -> io:format("~p\n", [M]),
             client()
    end.

loop() ->
    C = spawn(fun() -> client() end),
    N = spawn(fun() -> nd() end),
    loop(N, C).

loop(Node, Client) ->
    {ok, [V]} = io:fread("new value: ","~d"),
    case V of
        0 -> ok;
        V -> Node!#msg{type=insert, val=V, client=Client},
             loop(Node, Client)
    end.
