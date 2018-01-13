:- module(reader, [read_str/2]).

read_str(String, Form) :-
    tokenizer(String, Tokens),
    parse(Tokens, Form).


ws --> ` `, !, ws1.
ws --> `,`, !, ws1.
ws1 --> ` `, !, ws1.
ws1 --> `,`, !, ws1.
ws1 --> !.


splice_unquote --> `~@`.
lbracket --> `[`.
rbracket --> `]`.
lbrace --> `{`.
rbrace --> `}`.
lpar --> `(`.
rpar --> `)`.
syntax_quote --> `\``.
unquote --> `~`.
quote --> [39]. % single quote
with_meta --> `^`.
deref --> `@`.


no_backslash_or_quote([H|T]) --> [H], {H =\= 34 /* double quote */, H =\= 92 /* backslash */}, !, no_backslash_or_quote(T).
no_backslash_or_quote([]) --> !.

string1(Res, Acc) --> no_backslash_or_quote(T), [92, 34] /* escaped doublequote */, !, string1(Res, [[92, 34], T|Acc]).
string1(Res, Acc) --> no_backslash_or_quote(T), [92, C] /* escaped not-doublequote */, !, string1(Res, [[92, C], T|Acc]).
string1(Res, Acc) --> no_backslash_or_quote(T), [34] /* doublequote, end of string */, !, {reverse([T|Acc], Strings), append(Strings, Res)}.
string1(_, _) --> { throw(parse_error(`expected '"', got EOF`))}.
string(X) --> [34] /* double quote */,  string1(X, []).

ignore --> [_], !, ignore.
ignore --> !.
comment --> `;`, ignore.

nonspecial_characters([H|T]) --> [H],  { \+ member(H, `~@[](){}\`^; ,`), H =\= 34, H =\= 92 }, !, nonspecial_characters1(T).
nonspecial_characters1([H|T]) --> [H], { \+ member(H, `~@[](){}\`^; ,`), H =\= 34, H =\= 92 }, !, nonspecial_characters1(T).
nonspecial_characters1([]) --> !.

tokenize([splice_unquote|T]) --> splice_unquote, !, tokenize(T).
tokenize([lbracket|T]) --> lbracket, !, tokenize(T).
tokenize([rbracket|T]) --> rbracket, !, tokenize(T).
tokenize([lbrace|T]) --> lbrace, !, tokenize(T).
tokenize([rbrace|T]) --> rbrace, !, tokenize(T).
tokenize([lpar|T]) --> lpar, !, tokenize(T).
tokenize([rpar|T]) --> rpar, !, tokenize(T).
tokenize([syntax_quote|T]) --> syntax_quote, !, tokenize(T).
tokenize([unquote|T]) --> unquote, !, tokenize(T).
tokenize([quote|T]) --> quote, !, tokenize(T).
tokenize([with_meta|T]) --> with_meta, !, tokenize(T).
tokenize([deref|T]) --> deref, !, tokenize(T).
tokenize([string(S)|T]) --> string(S), !, tokenize(T).
tokenize([identifier_or_num(X)|T]) --> nonspecial_characters(X), !, tokenize(T).
tokenize(L) --> comment, !, tokenize(L).
tokenize(L) --> ws, !, tokenize(L).
tokenize([]) --> !.



tokenizer(String, Tokens) :-
    tokenize(Tokens, String, []).


form(list(L)) --> [lpar], !, list(L).
form(vector(L)) --> [lbracket], !, vector(L).
form(hashmap(L)) --> [lbrace], !, hashmap(L).
form(list([quote, X])) --> [quote], !, form(X).
form(list([unquote, X])) --> [unquote], !, form(X).
form(list([splice_unquote, X])) --> [splice_unquote], !, form(X).
form(list([syntax_quote, X])) --> [syntax_quote], !, form(X).
form(list([deref, X])) --> [deref], !, form(X).
form(list([with_meta, Y, X])) --> [with_meta], !, form(X), form(Y).
form(X) --> mal_atom(X).

char_is_digit(C) :-
    char_type(C, digit).

int(N) --> maplist(char_is_digit, X), number_codes(N, X).

mal_atom(int(X)) --> [identifer_or_num(X)], {int(X)}, !.
mal_atom(bool(true)) --> [identifier_or_num(`true`)], !.
mal_atom(bool(false)) --> [identifier_or_num(`false`)], !.
mal_atom(nil) --> [identifier_or_num(`nil`)], !.
mal_atom(string(X)) --> [string(X)].
% TODO: keyword, atom
mal_atom(symbol(X)) --> [identifier_or_num(X)].


list([H|T]) --> form(H), !, list(T).
list([]) --> [rpar], !.
list(_) --> {throw(parse_error(`expected ')', got EOF`))}.

vector([H|T]) --> form(H), !, vector(T).
vector([]) --> [rbracket], !.
vector(_) --> {throw(parse_error(`expected ']', got EOF`))}.

hashmap([H|T]) --> form(H), !, hashmap(T). % FIXME: literal should be two elements per entry
hashmap([]) --> [rbrace], !.
hashmap(_) --> {throw(parse_error(`expected '}', got EOF`))}.

parse([], nil) :- !. % for comments
parse(Tokens, AST) :-
    form(AST, Tokens, []).
