-module(useless).
-export([add/2, hello/0, greet/2, lol/1, zip/2]).

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
