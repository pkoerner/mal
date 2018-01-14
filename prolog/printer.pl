:- module(printer, [pr_str/2]).

interpose([H], _, [H]) :- !.
interpose([H|T], X, [H,X|R]) :-
    interpose(T, X, R).
interpose([], _, []).

pr_str(int(X), R) :-
    number_codes(X, R).
pr_str(symbol(X), X).
pr_str(string(X), Y) :-
    append([`"`, X, `"`], Y).
pr_str(keyword(X), Y) :-
    append([`:`, X], Y).
pr_str(bool(true), `true`).
pr_str(bool(false), `false`).
pr_str(nil, `nil`).
pr_str(list(L), R) :-
    maplist(pr_str, L, T),
    interpose(T, ` `, Tmp),
    append(Tmp, Joined),
    append([`(`, Joined, `)`], R).
pr_str(vector(L), R) :-
    maplist(pr_str, L, T),
    interpose(T, ` `, Tmp),
    append(Tmp, Joined),
    append([`[`, Joined, `]`], R).
pr_str(hashmap(L), R) :-
    maplist(pr_str, L, T),
    interpose(T, ` `, Tmp),
    append(Tmp, Joined),
    append([`{`, Joined, `}`], R).
pr_str(quote, `quote`).
pr_str(unquote, `unquote`).
pr_str(syntax_quote, `quasiquote`).
pr_str(splice_unquote, `splice-unquote`).
pr_str(deref, `deref`).
pr_str(with_meta, `with-meta`).
pr_str(X, _) :-
    print(printing_failed(X)),nl,fail.
