-module(useless).
-export([add/2, hello/0, greet/2, lol/1, zip/2, rpn/1, rocket/0]).

add(A, B) ->
    A + B + 1.

hello() ->
    io:format("hello world!\n").

greet(poznan, Name) ->
    io:format("Elo ~s mordeczko\n", [Name]);
greet(_, Name) ->
    io:format("hello, ~s\n", [Name]).

lol(X) when X == xd ->
    io:format("xd, indeed\n");
lol(_) ->
    io:format("not at all\n").

zip(X, Y) -> lists:reverse(zip(X, Y, [])).

zip([], _, Ans) -> Ans;
zip(_, [], Ans) -> Ans;
zip([X|Xs], [Y|Ys], Ans) -> zip(Xs, Ys, [{X, Y} | Ans]).

tokenize(S) -> case string:to_integer(S) of
                   {error, _} -> {op, S};
                   {Int, _} -> {num, Int}
               end.

rpn(X) -> rpn(lists:map(fun tokenize/1, string:tokens(X, " ")), []).

rpn([], Stack) -> hd(Stack);
rpn([{num, T}|Tokens], Stack) -> rpn(Tokens, [T|Stack]);
rpn([{op, "+"}|Tokens], [N1, N2|S]) -> rpn(Tokens, [N2 + N1|S]);
rpn([{op, "-"}|Tokens], [N1, N2|S]) -> rpn(Tokens, [N2 - N1|S]);
rpn([{op, "*"}|Tokens], [N1, N2|S]) -> rpn(Tokens, [N2 * N1|S]);
rpn([{op, "/"}|Tokens], [N1, N2|S]) -> rpn(Tokens, [N2 / N1|S]).

rocket() ->
    receive
        {From, liftoff} ->
            From!brwrbrwbrbwrbrwr;
        {From, second_stage} ->
            From!kyszu;
        {From, _} ->
            From!ground_control
    end.
