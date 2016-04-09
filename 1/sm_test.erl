%Simpson method

-module(sm_test).

-export([sm_test/0]).

-import(sm, [simpson/4]).

sm_test() -> 
    io:fwrite("Result for x^2 and [-100.0; 100.0] and delta = 0.001 is ~f~n", [simpson(-100.0, 101.0, fun(X) -> X * X end, 0.001)]).
