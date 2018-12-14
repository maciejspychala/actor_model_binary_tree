-module(tree).
-export([loop/0, logger/2]).

-record(node, {val=nil,
               left=nil,
               right=nil}).
-record(msg, {type,
              val,
              logger,
              id}).

nd() -> nd(#node{}).
nd(Node) ->
    receive
        M = #msg{type=insert} ->
            New = insert(M, Node),
            nd(New);
        M = #msg{type=lookup} ->
            lookup(M, Node),
            nd(Node);
        M = #msg{type=remove} ->
            New = remove(M, Node),
            nd(New)
    end.

log(M, Type) -> M#msg.logger!M#msg{type=Type}.

insert(M, N = #node{val=nil}) ->
    New = N#node{val=M#msg.val},
    log(M, inserted),
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
    log(M, found);
lookup(M, N) ->
    Branch = if M#msg.val < N#node.val -> N#node.left;
                M#msg.val >= N#node.val -> N#node.right
             end,
    case Branch of
        nil -> log(M, not_found);
        _ -> Branch!M
    end.

remove(M, N) when M#msg.val == N#node.val ->
    case N of
        #node{left=nil, right=nil} ->
            log(M, removed),
            N#node{val=nil}
    end;
remove(M, N) ->
    Branch = if M#msg.val < N#node.val -> N#node.left;
                M#msg.val >= N#node.val -> N#node.right
             end,
    case Branch of
        nil -> log(M, not_removed);
        _ -> Branch!M
    end,
    N.

process_messages(Id, []) -> {Id, []};
process_messages(Id, Q) ->
    [M|Ms] = lists:sort(fun(A, B) -> A#msg.id < B#msg.id end, Q),
    case M of
        #msg{id=Id} ->
            io:format("~p\n", [M]),
            process_messages(Id + 1, Ms);
        _ ->
            {Id, [M|Ms]}
    end.

logger(TId, TQ) ->
    {Id, Q} = process_messages(TId, TQ),
    receive
        M -> case M#msg.id of
                 Id ->
                     io:format("~p\n", [M]),
                     logger(Id + 1, Q);
                 _ ->
                     logger(Id, [M|Q])
             end
    end.

loop() ->
    C = spawn(fun() -> logger(0, []) end),
    N = spawn(fun() -> nd() end),
    loop(N, C, 0).

loop(Node, Logger, Id) ->
    {ok, [T]} = io:fread("type: ","~c"),
    {ok, [V]} = io:fread("value: ","~d"),
    case T of
        "i" ->
            Node!#msg{type=insert, val=V, logger=Logger, id=Id},
            loop(Node, Logger, Id + 1);
        "l" ->
            Node!#msg{type=lookup, val=V, logger=Logger, id=Id},
            loop(Node, Logger, Id + 1);
        "d" ->
            Node!#msg{type=remove, val=V, logger=Logger, id=Id},
            loop(Node, Logger, Id + 1)
    end.
