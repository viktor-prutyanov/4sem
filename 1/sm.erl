%Simpson method

-module(sm).

-export([simpson/4]).

sm_substep(A, B, Func) -> 
    (B - A) * (Func(A) + 4 * Func((A + B) / 2) + Func(B)) / 6.

sm_step(A, B, Func, Delta) when A < B ->
    sm_substep(A, A + Delta, Func) + sm_step(A + Delta, B, Func, Delta).

simpson(A, B, Func, Delta) -> sm_step(A, B, Func, Delta).

