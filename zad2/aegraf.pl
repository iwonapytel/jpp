:- use_module(library(lists)).

znajdzWierzcholek([[V | L] | _], V, [V | L]) :- !.
znajdzWierzcholek([_ | T], V, Vertex) :- znajdzWierzcholek(T, V, Vertex).

wyborWierzcholka([X, T], [X, T]).
wyborWierzcholka([X, e | Y1], [X, e, Y2])  :- member(Y2, Y1).
wyborWierzcholka([X, a | Y1], [X, a | Y2]) :- permutation(Y2, Y1), !.

% jestWyborem(+AEgraf, -Graf)
jestWyborem(_, []).
jestWyborem([X | R1], [Y | R2]) :- wyborWierzcholka(X, Y), !, jestWyborem(R1, R2).
jestWyborem(AEgraf, [[V | Tail] | R]) :-
	znajdzWierzcholek(AEgraf, V, Vertex),
	wyborWierzcholka(Vertex, [V | Tail]),
	jestWyborem(AEgraf, R).

% ?- jestWyborem([[v1, a, v2, v3], [v2, a, v1, v4]], X).
% ?- jestWyborem([[v2, a, v1, v3], [v1, a, v2, v3]], [[v1, a, v2, v3], [v2, a, v1, v3]]).

% jestDFS(+Graf, -Lista)


jestDFS([], []).
jestDFS([[V | X] | Y], Lista) :- doDFS([[V | X] | Y], znajdzSasiadowAE, [V], [], Lista).

% DFS(Graf, Stos, Odwiedzone, Lista)

doDFS(_, _, [], L, L).
doDFS(Graf, ZnajdzSasiadow, [S | Stos], Odwiedzone, Lista) :-
	\+ member(S, Odwiedzone), !,
	append(Odwiedzone, [S], NOdwiedzone),
	call(ZnajdzSasiadow, Graf, S, Sasiedzi),
	append(Sasiedzi, Stos, NStos),
        doDFS(Graf, ZnajdzSasiadow, NStos, NOdwiedzone, Lista).
doDFS(Graf, ZnajdzSasiadow, [S | Stos], Odwiedzone, Lista) :-
	member(S, Odwiedzone),
	doDFS(Graf, ZnajdzSasiadow, Stos, Odwiedzone, Lista).
	

%?- doDFS([[v0, a, v1, v3], [v1, e, v3], [v2, a], [v3, a]], [v0], [], X).

znajdzSasiadowAE([[V, _ | L1] | _], V, L1) :- !.
znajdzSasiadowAE([_ | L2], V, Sasiedzi) :- znajdzSasiadowAE(L2, V, Sasiedzi).

% todo zwroc dowolny, a nie tylko pierwszy 
znajdzSasiadowWybor([[V, a| L1] | _], V, L1).
znajdzSasiadowWybor([[V, e] | _], V, []).
znajdzSasiadowWybor([[V, e, X | _]| _], V, [X]).
znajdzSasiadowWybor([_ | R], V, Sasiedzi) :- znajdzSasiadowWybor(R, V, Sasiedzi).

% JestADFS(+AEgraf, -Lista)

jestADFS([], []).
jestADFS(AEgraf, Lista) :-
	jestWyborem(AEgraf, Wybor),
	jestDFS(Wybor, Lista).

% JestADFS1(+AEgraf, -Lista)
jestADFS1([], []).
jestADFS1([[V | X] | Y], Lista) :-
	doDFS([[V | X] | Y], znajdzSasiadowWybor, [V], [], Lista). 	
	
% jestADFS1([[v1, a, v2], [v2, e, v3, v4, v5], [v3, e], [v4, e], [v5, e]], X).
