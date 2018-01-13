:- module(environment, [empty_env/1, repl_env/1, lookup/3, store/4]).

:- use_module(library(assoc)).

empty_env(X) :-
    empty_assoc(X).
store(Env, K, V, NewEnv) :-
    put_assoc(K, Env, V, NewEnv).
lookup(Env, K, V) :-
    get_assoc(K, Env, V).

repl_env(X) :-
    empty_env(Empty),
    store(Empty,  `+`, builtin(args([A1,B1]), add(A1,B1)), EnvP),
    store(EnvP,   `-`, builtin(args([A2,B2]), sub(A2,B2)), EnvPM),
    store(EnvPM,  `*`, builtin(args([A3,B3]), mul(A3,B3)), EnvPMM),
    store(EnvPMM, `/`, builtin(args([A4,B4]), div(A4,B4)), X).
