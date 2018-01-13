% trivial functions
readd(X, X).
eval(X,X).
printt(X,R) :- name(R, X).

rep(X, R) :-
    readd(X, Y),
    eval(Y, Z),
    printt(Z, R).

% main loop
main :-
    write_term('user> ', []),
    read_line_to_codes(user_input, Line),
    (Line == end_of_file
     -> true % handle CTRL-D: be done
      ; rep(Line, Result), write_term(Result,[nl(true),character_escapes(false)]), !, main).
