:- module(step1_read_print, [main/0]).

:- use_module(reader, [read_str/2]).
:- use_module(printer, [pr_str/2]).

% trivial functions
readd(X, Y) :-
    read_str(X, Y).
eval(X,X).
printt(X,R) :- pr_str(X, T), name(R, T).

rep(X, R) :-
    readd(X, Y),
    eval(Y, Z),
    printt(Z, R).

% main loop

main :-
    write_term('user> ', []),
    catch((read_line_to_codes(user_input, Line) ,
           (Line == end_of_file
             -> halt % handle CTRL-D: be done
              ; rep(Line, Result), write_term(Result,[nl(true),character_escapes(false)]), !)),
          error(Reason),
          format('~s~n', [Reason])), !,
    main.
