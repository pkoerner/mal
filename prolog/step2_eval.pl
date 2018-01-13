:- module(step2_eval, [main/0]).

:- use_module(reader, [read_str/2]).
:- use_module(printer, [pr_str/2]).
:- use_module(environment, [repl_env/1, lookup/3]).

% trivial functions
readd(X, Y) :-
    read_str(X, Y).
eval(list([]), Env, list([]), Env) :- !.
eval(list([H|T]), Env, Res, REnv) :- !,
    eval_ast(list([H|T]), Env, [Fn|Args], REnv),
    call_fn(Fn, Args, Env, REnv, Res).
eval(X, Env, R, Env) :- /* TODO: the environment should not change here, right? */
    eval_ast(X, Env, R, Env).
printt(X,R) :- pr_str(X, T), name(R, T).

reduce_eval([], Env, [], Env).
reduce_eval([H|T], Env, [HR|TR], REnv) :-
    eval(H, Env, HR, TEnv),
    reduce_eval(T, TEnv, TR, REnv).


call_fn(builtin(args(Args), add(int(L), int(R))), Args, Env, Env, int(Res)) :-
    Res is L + R.
call_fn(builtin(args(Args), sub(int(L), int(R))), Args, Env, Env, int(Res)) :-
    Res is L - R.
call_fn(builtin(args(Args), mul(int(L), int(R))), Args, Env, Env, int(Res)) :-
    Res is L * R.
call_fn(builtin(args(Args), div(int(L), int(R))), Args, Env, Env, int(Res)) :-
    Res is L // R.


eval_ast(symbol(Sym), Env, Res, Env) :- !,
    (lookup(Env, Sym, T)
      -> copy_term(T, Res)
       ; append([`'`, Sym, `', not found`], Msg), throw(error(Msg))).
eval_ast(list(L), Env, Res, REnv) :- !,
    reduce_eval(L, Env, Res, REnv).
eval_ast(vector(L), Env, vector(Res), REnv) :- !,
    reduce_eval(L, Env, Res, REnv).
eval_ast(hashmap(L), Env, hashmap(Res), REnv) :- !,
    reduce_eval(L, Env, Res, REnv).
eval_ast(X, Env, X, Env).



rep(X, Env, R, REnv) :-
    readd(X, Y),
    eval(Y, Env, Z, REnv),
    printt(Z, R).

% main loop

main :-
    repl_env(Env),
    main(Env).
main(Env) :-
    write_term('user> ', []),
    catch((read_line_to_codes(user_input, Line) ,
           (Line == end_of_file
             -> halt % handle CTRL-D: be done
              ; rep(Line, Env, Result, NewEnv), write_term(Result,[nl(true),character_escapes(false)]), !)),
          error(Reason),
          (format('~s~n', [Reason]), NewEnv = Env)), !,
    main(NewEnv).



