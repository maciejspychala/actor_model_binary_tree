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
            nd(New);
        M = #msg{type=lookup} ->
            lookup(M, Node),
            nd(Node)
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

lookup(M, N) when M#msg.val == N#node.val ->
    M#msg.client!M#msg{type=found};
lookup(M, N) ->
    Branch = if M#msg.val < N#node.val -> N#node.left;
                M#msg.val >= N#node.val -> N#node.right
             end,
    case Branch of
        nil -> M#msg.client!M#msg{type=not_found};
        _ -> Branch!M
    end.

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
    {ok, [T]} = io:fread("type: ","~c"),
    {ok, [V]} = io:fread("new value: ","~d"),
    case T of
        "i" ->
            Node!#msg{type=insert, val=V, client=Client},
            loop(Node, Client);
        "l" ->
            Node!#msg{type=lookup, val=V, client=Client},
            loop(Node, Client)
    end.
