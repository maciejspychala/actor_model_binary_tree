-module(tree).
-export([loop/0, client/2]).

-record(node, {val=nil,
               left=nil,
               right=nil}).
-record(msg, {type,
              val,
              client,
              id}).

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

client(Id, Q) ->
    receive
        M -> case M#msg.id of
                 Id ->
                     io:format("~p\n", [M]),
                     client(Id + 1, Q);
                 _ ->
                     client(Id, [M|Q])
             end
    end.

loop() ->
    C = spawn(fun() -> client(0, []) end),
    N = spawn(fun() -> nd() end),
    loop(N, C, 0).

loop(Node, Client, Id) ->
    {ok, [T]} = io:fread("type: ","~c"),
    {ok, [V]} = io:fread("new value: ","~d"),
    case T of
        "i" ->
            Node!#msg{type=insert, val=V, client=Client, id=Id},
            loop(Node, Client, Id + 1);
        "l" ->
            Node!#msg{type=lookup, val=V, client=Client, id=Id},
            loop(Node, Client, Id + 1)
    end.
