% Practico 6
% LD es una lista de diferencias equivalente a la lista L.
l_ld(L,L-[]).


% L es la lista equivalente a la lista de diferencias LD.
ld_l(L-R,Lres) :- length(L,X), length(R,Y), Largo is X-Y, copiarLista(L,Largo,Lres).

copiarLista(_,0,[]).
copiarLista([X|R1],Largo,[X|R2]) :- Ln is Largo-1, copiarLista(R1,Ln,R2).

% Ej3

strange(Xs, Ys, P) :- strange(Xs, Ys, [], P).
strange([X|Xs], [X|Ys], Zs, P) :- X<P, strange(Xs, Ys, Zs, P).
strange([X|Xs], Ys, Zs, P) :- strange(Xs, Ys, [X|Zs], P).
strange([], Xs, Xs, _).


par(X) :- 0 is (X mod 2), !.
multiplo_tres(X) :- 0 is (X mod 3).
sel_par([X|Xs], X, Z) :- par(X), X > Z.
sel_par([X|Xs], Y, Z) :- sel_par(Xs, Y, Z).
prob(X) :- sel_par([1,4,12,5,32,30,17,18], X, 10),
	   multiplo_tres(X).

% Se satisface si alguno de los argumentos lo hace.
or(Goal1,_) :- Goal1, !.
or(_,Goal2) :- Goal2.

% Se satisface si Goal tiene un árbol SLD
% finitamente fallado (según la regla de
% computación de Prolog).
not(Goal) :- Goal, !, fail.
not(_) :- !.

% Se satisface una única vez, si Goal se satisface al menos una vez.
once2(Goal) :- Goal, !.

% Análogo al predicado once, pero se satisface siempre, aunque Goal no
% lo haga.
ignore(Goal) :- Goal, !.
ignore(_) :- !.

% Se satisface si If y Then se satisfacen.
if_then(If, _) :- not(If), !, fail.
if_then(_, Then) :- Then.

% Se satisface si If y Then se satisfacen, o si no se
% satisface If, pero sí lo hace Else.
if_then_else(If, _, Else) :- not(If), !, Else.
if_then_else(_, Then, _) :- Then.


% Practico 7

padre(juan, ana).
padre(juan, jose).
padre(juan, pedro).
padre(pedro, hector).
padre(pedro, gustavo).
padre(hector, maria).

% L es la sublista más larga común a L1 y L2.
max_comun(L1, L2, L) :- setof(X,(sublista(X,L1),sublista(X,L2)),L3),
	                max_comun(L3,0,[],L).
max_comun([],_,Acc,Acc).
max_comun([X|R],LargoMax,_,L) :- length(X,LargoX), LargoX > LargoMax, !,
				 max_comun(R,LargoX,X,L).
max_comun([_|R],LargoMax,Acc,L) :- max_comun(R,LargoMax,Acc,L).

%choose(+N,?L,?Sub)
%Sub es una subsecuencia de largo N de la lista L.

choose(0,_,[]).
choose(N,[C|R],[C|R2]):- N>0, I is N-1, length(R2,I), choose(I,R,R2).
choose(N,[_|R],R2):- N>0, length(R2,N), choose(N,R,R2).


sublista(Sub,Lista):- sufijo(Sufijo,Lista), prefijo(Sub,Sufijo).
sufijo(S,L):- append(_,S,L).
prefijo(P,L):- append(P,_,L).

% C es la unión de los conjuntos C1 y C2.
union(C1, C2, C) :- setof(X,(member(X,C1) ; member(X,C2)), C).

% C es la intersección de los conjuntos C1 y C2.
intersección(C1, C2, C) :-  setof(X,(member(X,C1) , member(X,C2)), C).

% C es igual a C1-C2.
diferencia(C1, C2, C) :- setof(X,(member(X,C1) , not(member(X,C2))), C).

between(M,N,M) :- M=<N.
between(M,N,K) :- M<N,
	          M2 is M+1,
                  between(M2,N,K).
par2(N) :- N mod 2 =:= 0.
par_menor(N,M) :- between(1,N,M),
	          par2(M).
todos_q(Q,Xs) :- findall(X,(append(Q,[X],TL), T=..TL, call(T)),Xs).




% Implemente el predicado findall utilizando los predicados assert y retract.
findall_custom(Template,Goal,Bag) :-
	call(Goal),
	assert(queue(Template)),
	fail;
	assert(queue(bottom)),
	recolectar(Bag).

recolectar(L) :-
	retract(queue(X)), !,
	( X == bottom, !, L = []
	;
	 L = [X|R], recolectar(R)).



% Practico 8

oracion --> gn(1), gv(1).
oracion --> gn(1), gv(2).
oracion --> gn(4), gv(3).
oracion --> gn(6), gv(4).
gn(1) --> art(1), sust(1).
gn(2) --> sust(2).
gn(3) --> art(3), art(4), sust(3).
gn(4) --> art(2), sust(4).
gn(5) --> sust(5), art(5), art(9), sust(1).
gn(6) --> art(1), sust(6).
gn(7) --> art(7), sust(7), art(8), art(6), sust(8).
gv(1) --> verbo(1), gn(2).
gv(2) --> verbo(2), gn(3).
gv(3) --> verbo(4), gn(5).
gv(4) --> verbo(3), gn(7).

art(1) --> ['Los'].
art(2) --> ['El'].
art(3) --> [en].
art(4) --> [la].
art(5) --> [a].
art(6) --> [las].
art(7) --> [el].
art(8) --> [de].
art(9) --> [los].
sust(1) --> [osos],[polares].
sust(2) --> [peces].
sust(3) --> ['Antartida'].
sust(4) --> [investigador].
sust(5) --> [alimento].
sust(6) --> [investigadores].
sust(7) --> [comportamiento].
sust(8) --> [aves].
verbo(1) --> [comen].
verbo(2) --> [viven].
verbo(3) --> [estudian].
verbo(4) --> [da].

% Ej3 L = {a^nb^mc^n+m / n,m>0}

l --> a(N), b(M), c(Suma), {Suma is N+M}.
a(N) --> [a], a(N1), {N is N1+1}.
a(1) --> [a].
b(M) --> [b], b(M1), {M is M1+1}.
b(1) --> [b].
c(S) --> [c], c(S1), {S is S1+1}.
c(2) --> [c,c].


% Ejemplo, S = {a^n b^n c^n}
s --> a2(N), b2(N), c2(N).
a2(N) --> [a], a2(N1), {N is N1 + 1}.
a2(0) --> [].
b2(N) --> [b], b2(N1), {N is N1 + 1}.
b2(0) --> [].
c2(N) --> [c], c2(N1), {N is N1 + 1}.
c2(0) --> [].


% Ej4

indice(Patron,Lista,Indice) :- indice(Patron,1,Indice,Lista,_).
indice(Patron,I,I) --> Patron.
indice(Patron,I0,I) --> [_],{I1 is I0+1},indice(Patron,I1,I).

% Metainterpretes

% resDerecha(+G)
% resuelve G con la siguiente regla de computación:
% siempre se selecciona el átomo de más a la derecha
% para la sustitución.

resDerecha(Goal) :-
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	Body == true.

resDerecha(Goal) :-
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	functor(Body,F,_),
	F \= ',',
	resDerecha(Body).

resDerecha(Goal) :-
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	arg(1,Body,Arg1),
	arg(2,Body,Arg2),
	resDerecha(Arg2),
	resDerecha(Arg1).


arco(a, b).
arco(b, c).
arco(b, d).
camino(X, Y) :- arco(X, Y).
camino(X, Y) :- arco(X, Z), camino(Z, Y).


% resuelve G siempre que el árbol de la prueba no
% supere la profundidad Max. Cada vez que se llegue a
% un árbol de profundidad N, el intérprete debe
% imprimir el siguiente mensaje en la salida estándar:
% «Cuidado, árbol muy profundo».

resAviso(Goal,N,Max) :-
	nb_setval(profundidad,0),
	resAviso2(Goal,N,Max).

resAviso2(Goal,N,Max) :-
	nb_getval(profundidad,Valor),
	(Valor == N, write('«Cuidado, árbol muy profundo»'), nl
	;
	 Valor < Max),
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	Body == true.

resAviso2(Goal,N,Max) :-
	nb_getval(profundidad,Valor),
	(Valor == N, write('«Cuidado, árbol muy profundo»'), nl
	;
	 Valor < Max),
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	functor(Body,F,_),
	F \= ',',
	ValorN is Valor+1,
	nb_setval(profundidad,ValorN),
	resAviso2(Body,N,Max),
	nb_setval(profundidad,Valor).

resAviso2(Goal,N,Max) :-
	nb_getval(profundidad,Valor),
	(Valor == N, write('«Cuidado, árbol muy profundo»'), nl
	;
	 Valor < Max),
	callable(Goal), Goal \= true,
	clause(Goal,Body),
	arg(1,Body,Arg1),
	arg(2,Body,Arg2),
	ValorN is Valor+1,
	nb_setval(profundidad,ValorN),
	resAviso2(Arg1,N,Max),
	nb_setval(profundidad,ValorN),
	resAviso2(Arg2,N,Max),
	nb_setval(profundidad,Valor).












