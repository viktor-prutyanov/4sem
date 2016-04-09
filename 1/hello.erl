%factorial program

-module(hello).
-export([factorial/1]).
-export([dfact/0]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

dfact() -> io:fwrite("Double factorial is ~.B~n", [factorial(7)]).
