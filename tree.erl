-module(tree).
-export([loop/0, logger/2]).

-record(node, {val=nil,
               left=nil,
               right=nil}).
-record(msg, {type,
              val,
              logger,
              id,
              req}).

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
            nd(New);
        M = #msg{type=annihilate} ->
            M#msg.req!#msg{type=annihilated, val=Node};
        M = #msg{type=most_right} ->
            New = most_right(M, Node),
            nd(New)
    end.

log(M, Type) -> M#msg.logger!M#msg{type=Type}.

most_right(M, N = #node{right=nil}) ->
    M#msg.req!#msg{type=most_right_found, val=N#node.val},
    io:format("MOST RIGHT ~p~n", [N]),
    remove(M#msg{logger=self(), val=N#node.val}, N);
most_right(M, N) ->
    N#node.right!M,
    N.

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
                M#msg.val >= N#node.val -> N#node.right;
                true -> nil
             end,
    case Branch of
        nil -> log(M, not_found);
        _ -> Branch!M
    end.

remove(M, N) when M#msg.val == N#node.val ->
    case N of
        #node{left=nil, right=nil} ->
            log(M, removed),
            N#node{val=nil};
        #node{left=nil} ->
            N#node.right!#msg{type=annihilate, req=self()},
            receive
                #msg{type=annihilated, val=New} ->
                    log(M, removed),
                    New
            end;
        #node{right=nil} ->
            N#node.left!#msg{type=annihilate, req=self()},
            receive
                #msg{type=annihilated, val=New} ->
                    log(M, removed),
                    New
            end;
        _ ->
            N#node.left!#msg{type=most_right, req=self()},
            receive
                #msg{type=most_right_found, val=Node} ->
                    log(M, removed),
                    N#node{val=Node}
            end
    end;

remove(M, N) ->
    Branch = if N#node.val == nil -> nil;
                M#msg.val < N#node.val -> N#node.left;
                M#msg.val >= N#node.val -> N#node.right;
                true -> nil
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
                     io:format("~p: ~p\n", [M#msg.val, M#msg.type]),
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
    timer:sleep(1000),
    T = lists:nth(rand:uniform(3), ["i", "l", "d"]),
    V = rand:uniform(20),
    case T of
        "i" ->
            Node!#msg{type=insert, val=V, logger=Logger, id=Id},
            loop(Node, Logger, Id + 1);
        "l" ->
            Node!#msg{type=lookup, val=V, logger=Logger, id=Id},
            loop(Node, Logger, Id + 1);
        "d" ->
            Node!#msg{type=remove, val=V, logger=self(), id=Id},
            receive
                M = #msg{type=removed} -> Logger!M;
                M = #msg{type=not_removed} -> Logger!M
            end,
            loop(Node, Logger, Id + 1)
    end.
