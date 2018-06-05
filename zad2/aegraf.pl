:- use_module(library(lists)).

% znajdzWierzcholek(Graf, V, Vertex)
% Ostatni argument jest elementem grafu reprezentującym wierzchołek o symbolu V.
znajdzWierzcholek([Vertex | _], V, Vertex) :- Vertex = [V | _], !.
znajdzWierzcholek([_ | T], V, Vertex) :- znajdzWierzcholek(T, V, Vertex).

% wyborWierzcholka(+V1, -V2)
% Drugi argument jest wyborem pierwszego wierzchołka.
wyborWierzcholka([V, Type], [V, Type]).
wyborWierzcholka([V, e | N1], [V, e, N2])  :- member(N2, N1).
wyborWierzcholka([V, a | N1], [V, a | N2]) :- permutation(N2, N1), !.

% jestWyborem(+AEgraf, -Graf)
jestWyborem([], []).
% Generowanie wyboru dla nieustalonego argumentu X.
jestWyborem([V1 | T1], [V2 | T2]) :-
	wyborWierzcholka(V1, V2), !,
	jestWyborem(T1, T2).

jestWyborem(AEgraf, [[V | N] | T]) :-
	znajdzWierzcholek(AEgraf, V, Vertex),
	wyborWierzcholka(Vertex, [V | N]),
    delete(AEgraf, Vertex, _AEgraf),
	jestWyborem(_AEgraf, T).


% jestDFS(+Graf, -Lista)
jestDFS([], []).
jestDFS(Graf, Lista) :- Graf = [[V | _] | _], doDFS(Graf, znajdzSasiadowAE, [V], [], Lista).

% doDFS(Graf, ZnajdzSasiadow, Stos, Odwiedzone, Lista)
doDFS(_, _, [], L, L).

doDFS(Graf, ZnajdzSasiadow, [S | Stos], Odwiedzone, Lista) :-
	\+ member(S, Odwiedzone), !,
	append(Odwiedzone, [S], _Odwiedzone),
	call(ZnajdzSasiadow, Graf, S, Sasiedzi),
    subtract(Sasiedzi, Odwiedzone, _Sasiedzi),
    permutation(PermSasiadow, _Sasiedzi),
	append(PermSasiadow, Stos, _Stos),
    doDFS(Graf, ZnajdzSasiadow, _Stos, _Odwiedzone, Lista).

doDFS(Graf, ZnajdzSasiadow, [S | Stos], Odwiedzone, Lista) :-
	member(S, Odwiedzone),
	doDFS(Graf, ZnajdzSasiadow, Stos, Odwiedzone, Lista).

% znajdzSasiadow(+Graf, +V, -Sasiedzi)
% Znajdowanie sasiadow dla grafów AE i wyboru.
znajdzSasiadowAE(Graf, V, Sasiedzi) :- znajdzWierzcholek(Graf, V, [_, _ | Sasiedzi]).

znajdzSasiadowWybor(Graf, V, Sasiedzi) :- znajdzWierzcholek(Graf, V, [_, a | Sasiedzi]).
znajdzSasiadowWybor(Graf, V, []) :- znajdzWierzcholek(Graf, V, [_, e]).
znajdzSasiadowWybor(Graf, V, [N]) :-
	znajdzWierzcholek(Graf, V, [_, e | Sasiedzi]),
	member(N, Sasiedzi).

% JestADFS(+AEgraf, -Lista)
% Sprawdzanie, czy dana lista jest przejśćiem dfs w grafie wyboru wraz z jego tworzeniem.
jestADFS(AEgraf, Lista) :-
	jestWyborem(AEgraf, Wybor),
	jestDFS(Wybor, Lista).

% JestADFS1(+AEgraf, -Lista)
% To samo co w dfs, tylko bez tworzenia grafu wyboru.
jestADFS1([], []).
jestADFS1(AEgraf, Lista) :-
	AEgraf = [[V | _] | _],
	doDFS(AEgraf, znajdzSasiadowWybor, [V], [], Lista).
