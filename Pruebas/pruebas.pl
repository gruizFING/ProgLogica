

loop_solve(Obj,_) :- Obj \= true,
	             clause(Obj,Body),
	             Body == true.

loop_solve(Obj,H) :- functor(Obj,_,A),
	             A > 0, !,
		     arg(1,Obj,A1),
		     loop_solve(A1,H),
		     arg(2,Obj,A2),
		     loop_solve(A2,H).

loop_solve(Obj,H) :- functor(Obj,F,_),
		     member(Obj,H), !,
		     write('cuidado: loop'), nl,
		     write(F), nl,
		     fail.

loop_solve(Obj,H) :- Obj \= true,
	             clause(Obj,Body),
		     loop_solve(Body,[Obj|H]).


p2 :- a2,b2.
a2 :- b2.
a2 :- r2.
b2 :- p2.
b2 :- s2.
r2.
s2.

p(X) :- p(s(X)).

% Solucion de la prueba

loop_solve2(true,_):- !.
loop_solve2((A,B),Hist):-
	!,
	loop_solve2(A,Hist),
	loop_solve2(B,Hist).
loop_solve2(A,Hist) :-
	member(A,Hist),
	writeln('cuidado: loop '),
	writeln(A),
	!,
	fail.
loop_solve2(A,Hist):-
	clause(A,B),
	loop_solve2(B,[A|Hist]).


fila_completa_valida(N,F) :- length(F,N), fila_completa_valida(N,F,1).

fila_completa_valida(N,F,N) :- member(N,F), !.

fila_completa_valida(N,F,E) :- member(E,F),
	                       En is E+1,
			       fila_completa_valida(N,F,En).

fila_completa_valida2(N, F) :-
	fila_compl_val(F,N,[]).
fila_compl_val([],_,_) :- !. % ! verde
fila_compl_val([E|Es],N,Ac) :-
	between(1,N,E), % E es un valor entre 1 y N
	\+member(E,Ac),
	fila_compl_val(Es,N,[E|Ac]).


col_i_esima(M,I,C) :- length(M,N),
	              col_i_esima(M,I,1,N,C).
col_i_esima([F],I,N,N,[E]) :- nth1(I,F,E), !.
col_i_esima([F|R],I,E,N,[Elem|C]) :- nth1(I,F,Elem),
	                             En is E+1,
				     col_i_esima(R,I,En,N,C).

sudoku_simple(N,M) :- generar_matrix(N,1,M), check_matrix(N,1,M).

generar_matrix(N,X,[]) :- X is N+1, !.
generar_matrix(N,X,[F|R]) :- fila_completa_valida(N,F),
			     Xn is X+1,
			     generar_matrix(N,Xn,R).

check_matrix(N,X,_) :- X is N+1, !.
check_matrix(N,X,M) :- col_i_esima(M,X,C),
		       fila_completa_valida(N,C),
		       Xn is X+1,
		       check_matrix(N,Xn,M).


p3(_).

p3(X) :- !, X is 2.

subterm(Term,Term).
subterm(Sub,Term):-
	compound(Term),
	Term=..[_F|Args],
	subtermList(Sub,Args).

subtermList(Sub,[Arg|_Args]):-
	subterm(Sub,Arg).
subtermList(Sub,[_Arg|Args]):-
	subtermList(Sub,Args).


p4(X) :- p4(f2(X)), q4(X).
p4(f2(b)).
q4(b).


g --> [].
g --> [a],w,[a],f.
g --> [b],w,[b],f.
w --> [a],w,[a].
w --> [b],w,[b].
w --> a,b.
b,[a] --> b,c.
b,[b] --> b,d.
c,[a] --> [a],c.
c,[b] --> [b],c.
c,f --> f,[a].
d,[a] --> [a],d.
d,[b] --> [b],d.
d,f --> f,[b].
a,b,f --> [].


z(true) :- !.
z((A,B)) :- !, z(A), z(B).
z(G) :- predicate_property(G,built_in), !, G.
z(G) :- \+clause(G,_), !, write('falla - '), writeln(G), fail.
z(G) :-	clause(G,Body), z(Body).


p5(a).
p5(f(f(X))) :- p5(X).
r5([],N,[]) :- p5(N).
r5([X|Xs],f(N),[X|Ys]) :- r5(Xs,N,Ys), !.
r5([_|Xs],f(N), Ys) :- r5(Xs, f(N),Ys).
