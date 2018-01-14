:- module(environment, [empty_env/1, repl_env/1,
                        lookup/3, find/4, store/4,
                        store_deep/4,
                        push_scope/2, pop_scope/2]).

:- use_module(library(assoc)).

pop_scope(env_outer(_, OldEnv), OldEnv).
push_scope(OldEnv, env_outer(EmptyAssoc, OldEnv)) :-
    empty_assoc(EmptyAssoc).
empty_env(env_outer(X, nil)) :-
    empty_assoc(X).
/* named set in the guide */
store(env_outer(Env, Outer), K, V, env_outer(NewEnv, Outer)) :-
    put_assoc(K, Env, V, NewEnv).
store_deep(env_outer(Env, nil), K, V, env_outer(REnv, nil)) :- !,
    store(env_outer(Env, nil), K, V, env_outer(REnv, nil)).
store_deep(env_outer(Env, Outer), K, V, env_outer(Env, Router)) :-
    store_deep(Outer, K, V, Router).
/* named get in the guide */
lookup(env_outer(Env, _), K, V) :-
    get_assoc(K, Env, V).

find(env_outer(Env, Outer), K, env_outer(Env, Outer), V) :-
    lookup(env_outer(Env, Outer), K, V), !.
find(env_outer(_, Outer), K, REnv, V) :-
    find(Outer, K, REnv, V).
find(nil, _, _, _) :-
    throw(error(`not found`)).




repl_env(X) :-
    empty_env(Empty),
    store(Empty,  `+`, builtin(args([A1,B1]), add(A1,B1)), EnvP),
    store(EnvP,   `-`, builtin(args([A2,B2]), sub(A2,B2)), EnvPM),
    store(EnvPM,  `*`, builtin(args([A3,B3]), mul(A3,B3)), EnvPMM),
    store(EnvPMM, `/`, builtin(args([A4,B4]), div(A4,B4)), X).
