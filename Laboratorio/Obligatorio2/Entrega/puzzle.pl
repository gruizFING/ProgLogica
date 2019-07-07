/*------- Puzzle solver --------*/

% nth(+Pos,?L,?E)
% E es el elemento en la posición Pos (contando a partir de 1) de la lista L.

nth(1,[X|_],X).
nth(P,[_|R],X) :- P > 1, Q is P-1, nth(Q,R,X).

% cell_at(+X,+Y,?M,?P)
% P es la celda en la fila X y columna Y de la matriz M, las filas y
% columnas comienzan en 1.

cell_at(1,Y,[C|_],P) :- nth(Y,C,P).
cell_at(X,Y,[_|R],P) :- X > 1, Y > 0, I is X-1, cell_at(I,Y,R,P).

% match(+C,+X,+Ficha,+R)
% C corresponde a un color de una ficha
% X es el lado de la ficha a comparar
% Ficha es la ficha con la que se compara
% R es la rotacion de Ficha
% Se rota Ficha segun R y luego se compara el lado X con el Color C

match(C,X,Ficha,0) :- nth(X,Ficha,C).
match(C,X,[F1,F2,F3,F4,_],R) :- R>0, Rn is R-1, match(C,X,[F4,F1,F2,F3,_],Rn).

% addPosiciones(+F,+P,?FP)
% F es la lista de fichas a agregarles las posiciones
% P es la posicion actual que se encuentra
% FP es el resultado de agregarles la posicion a cada ficha de F
% Con este predicado se le agrega a cada ficha, representada como una
% 4-tupla de Colores, la posicion en la lista dejandola como una 5-tupla
% con los colores y la posicion
% Es necesario para los predicados que siguen ya que la lista original
% de fichas se modifica y se tiene que guardar la posicion original de
% cada ficha inicialmente

addPosiciones([[C1,C2,C3,C4]],P,[[C1,C2,C3,C4,P]]).
addPosiciones([[C1,C2,C3,C4]|R1],P,[[C1,C2,C3,C4,P]|R2]) :- Pn is P+1, addPosiciones(R1,Pn,R2).

% sacarFicha(+Fichas,+P,?Res)
% Elimina la ficha con la posicion P de Fichas y devuelve la lista de
% fichas resultante en Res
% La posicion P no se corresponde con la posicion en la Lista sino que
% con la posicion original de la ficha que se guarda en la 5-tupla
% mencionada anteriormente

sacarFicha([[_,_,_,_,P]|R],P,R).
sacarFicha([C|R],P,[C|F]) :- sacarFicha(R,P,F).

% solve_puzzle(+File,+N,?Solution)
% File es un archivo con los datos de entrada como se indican en la
% letra.
% N es el tamaño del puzzle a resolver.
% Solution es la solucion del puzzle en el formato indicado en la letra.
% Primero se abre el archivo File y se leen los datos dejando todo en
% FichasMatrix, luego se cierra el archivo y se le agregan las
% posiciones a cada ficha pasando como posicion inicial 1.
% Finalmente se llama al predicado que resuelve el puzzle.

solve_puzzle(File,N,Solution) :- open(File,read, FichasStream), read(FichasStream, FichasMatrix), close(FichasStream),
	                         addPosiciones(FichasMatrix,1,Fichas),
				 solve_puzzle_matrix(Fichas, Fichas, N, [N,N], [], Solution), !.

% solve_puzzle_matrix(+Fichas,+FichasO, +N, +[X,Y], +SolAc, ?Sol)
% Se arma la solucion empezando de la posicion [N,N], y se va llendo
% hacia la izquierda y arriba en la matriz resultado. Por esta razon se
% le pasa [N,N] inicialmente en el cuarto parametro.
% Se lleva una lista de fichas que se va modificando (Fichas) y otra que
% es la original (FichasO).
% Se pasa el tamaño del puzzle (N) y un acumulador de la solucion
% (SolAc), por eso se le pasa la lista vacia cuando arranca.
% El ultimo parametro (Sol) corresponde a la solucion final del puzzle.
% En cada paso se llama a get_ficha pasandole el caso que corresponda
% (Si es en el borde, en una esquina, etc), este predicado devuelve la
% posicion (Pos) y rotacion (Rot)de una ficha candidata a una solucion.
% Luego se saca de la lista de fichas que es modificable (Fichas) y se
% agrega los datos de la ficha a la solucion parcial (SolAc).
% Finalmente se llama recursivamente a solve_puzzle_matrix con los datos
% actualizados como las fichas disponibles (FichasN), la Solucion
% Acumulada y la posicion en la matriz solucion.
% Esto se repite hasta se llega a la posicion [1,1] que es cuando falta
% la ultima ficha para armar la solucion final, se busca igual que antes
% y si se encuentra se coloca en la solucion acumulada y se copia a la
% solucion final (Sol)


solve_puzzle_matrix(Fichas, FichasO, N, [N,N], SolAc, Sol) :- get_ficha(1, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[N,N,Pos,Rot]],SolAc,SolAcN),
							      Nn is N-1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [N,Nn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [N,C], SolAc, Sol) :- C>1, C<N,
	                                                      get_ficha(2, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[N,C,Pos,Rot]],SolAc,SolAcN),
							      Cn is C-1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [N,Cn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [N,1], SolAc, Sol) :- get_ficha(3, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[N,1,Pos,Rot]],SolAc,SolAcN),
							      Nn is N-1,
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [Nn,N], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,N], SolAc, Sol) :- F>1, F<N,
	                                                      get_ficha(4, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,N,Pos,Rot]],SolAc,SolAcN),
	                                                      Nn is N-1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [F,Nn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,C], SolAc, Sol) :- F>1, F<N, C>1, C<N,
	                                                      get_ficha(5, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,C,Pos,Rot]],SolAc,SolAcN),
	                                                      Cn is C-1,
						              solve_puzzle_matrix(FichasN, FichasO, N, [F,Cn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,1], SolAc, Sol) :- F>1, F<N,
	                                                      get_ficha(6, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
							      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,1,Pos,Rot]],SolAc,SolAcN),
							      Fn is F-1,
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [Fn,N], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [1,N], SolAc, Sol) :- get_ficha(7, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[1,N,Pos,Rot]],SolAc,SolAcN),
							      Nn is N-1,
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [1,Nn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [1,C], SolAc, Sol) :- C>1, C<N,
							      get_ficha(8, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
							      sacarFicha(Fichas,Pos,FichasN),
							      append([[1,C,Pos,Rot]],SolAc,SolAcN),
	                                                      Cn is C-1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [1,Cn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [1,1], SolAc, Sol) :- get_ficha(9, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
						              append([[1,1,Pos,Rot]],SolAc,Sol).


% get_ficha(+C,+Fichas,+FichasO,+Rot,+SolAc,?PosF,?RotF,+N)
% C corresponde al numero de caso del predicado, hay 9 casos que se
% correspondes con la ubicacion en la matriz solucion.
% Fichas, FichasO, SolAc y N son lo mismo que en el predicado anterior.
% Rot corresponde a la rotacion de la ficha que se esta viendo en el
% momento.
% PosF y RotF son la posicion y rotacion finales de la ficha candidata
% en la solucion.


% Caso 1: La ficha debe encajar en la posicion [N,N] de la solucion.
%	  Es la primer ficha que se mete en la solucion ya que se
%	  empieza a armar desde esta posicion.
%	  Se compara el lado derecho y el de abajo con 0, es decir su
%	  color C2 y C3 deben ser 0.

get_ficha(1,[[_,0,0,_,P]|_],_,R,_,P,R,_).

% Caso 2: Como se explico antes la solucion se va armando metiendo las
%	  piezas de la posicion [N,N] hacia la izquierda y arriba de la
%	  matriz
%	  En esta caso se verifica que el lado de abajo sea 0 y que
%	  coincidan los colores del lado derecho con el lado izquierdo
%	  de la ficha anterior en la solucion

get_ficha(2,[[_,C2,0,_,Pos]|_],FichasO,Rot,[[_,_,PF,RF]|_],Pos,Rot,_) :- nth(PF,FichasO,Ficha),
								         match(C2,4,Ficha,RF).

% Caso 3: Posicion [N,1], se verifica que los lados de abajo e
%	  izquierda sean 0 (C3yC4) y que coincidan los colores del lado
%	  derecho con el lado izquierdo de la ficha anterior en la
%	  solucion

get_ficha(3,[[_,C2,0,0,Pos]|_],FichasO,Rot,[[_,_,PF,RF]|_],Pos,Rot,_) :- nth(PF,FichasO,Ficha),
									 match(C2,4,Ficha,RF).

% Caso 4: Se verifica que el lado derecho sea 0 y que coindidan los
%	  colores del lado inferior con el lado superior de la ficha
%	  N veces antes de la solucion

get_ficha(4,[[_,0,C3,_,Pos]|_],FichasO,Rot,Sol,Pos,Rot,N) :- cell_at(N,3,Sol,PFicha), cell_at(N,4,Sol,RFicha),
							     nth(PFicha,FichasO,Ficha),
							     match(C3,1,Ficha,RFicha).

% Caso 5: Se verifica que coindidan los colores del lado derecho con el
%	  lado izquierdo de la ficha anterior en la solucion y que
%	  coindidan los colores del lado inferior con el lado superior
%	  de la ficha N veces antes en la solucion

get_ficha(5,[[_,C2,C3,_,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									     match(C2,4,Ficha1,RF1),
									     Nn is N-1,
									     cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									     nth(PF2,FichasO,Ficha2),
									     match(C3,1,Ficha2,RF2).

% Caso 6: Se verifica que coindidan los colores del lado derecho con el
%	  lado izquierdo de la ficha anterior en la solucion y que
%	  coindidan los colores del lado inferior con el lado superior
%	  de la ficha N veces antes en la solucion.
%	  Ademas como es el borde izquierdo tiene que tener color 0 en
%	  el lado izquierdo.

get_ficha(6,[[_,C2,C3,0,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									    match(C2,4,Ficha1,RF1),
									    Nn is N-1,
									    cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									    nth(PF2,FichasO,Ficha2),
									    match(C3,1,Ficha2,RF2).

% Caso 7: Posicion [1,N]. Como el caso 4 pero ademas se checkea que el
%	  lado superior tenga color 0

get_ficha(7,[[0,0,C3,_,Pos]|_],FichasO,Rot,Sol,Pos,Rot,N) :-  cell_at(N,3,Sol,PFicha), cell_at(N,4,Sol,RFicha),
							      nth(PFicha,FichasO,Ficha),
							      match(C3,1,Ficha,RFicha).

% Caso 8: Como el 5 y al ser borde de arriba de la solucion se verifica
%         ademas que el lado superior sea 0.

get_ficha(8,[[0,C2,C3,_,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									     match(C2,4,Ficha1,RF1),
									     Nn is N-1,
									     cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									     nth(PF2,FichasO,Ficha2),
									     match(C3,1,Ficha2,RF2).
% Caso 9: Posicion [1,1], el caso final es como el 8 ademas checkeando
%	  que el lado izquierdo sea 0.

get_ficha(9,[[0,C2,C3,0,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									     match(C2,4,Ficha1,RF1),
									     Nn is N-1,
									     cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									     nth(PF2,FichasO,Ficha2),
									     match(C3,1,Ficha2,RF2).

% Si no matchea algunos de los casos con la ficha actual, entonces se
% rota la ficha.
% Si ya fue rotada 270 grados entonces se descarta y se
% prueba con la siguiente

get_ficha(C,[[C1,C2,C3,C4,P]|RestoFichas],FichasO,Rot,Sol,PosF,RotF,N) :- Rot<3, Rnuevo is Rot+1,
							  get_ficha(C,[[C4,C1,C2,C3,P]|RestoFichas],FichasO,Rnuevo,Sol,PosF,RotF,N).

get_ficha(C,[_|RestoFichas],FichasO,3,Sol,PosF,RotF,N) :- get_ficha(C,RestoFichas,FichasO,0,Sol,PosF,RotF,N).



/*------ Metainterpreter -------*/

% profiler(+Goal,-Profile

delete_member_pos(1,E,[E|R],R).
delete_member_pos(P,E,[_|R],Res) :- P > 1, Q is P-1, delete_member_pos(Q,E,R,Res).


delete_member(E,[E|R],R).
delete_member(E,[X|R],[X|Res]) :- delete_member(E,R,Res).


mi1(true,_)  :- !.

mi1((A,B),AuxProf) :- !, mi1(A,AuxProf), mi1(B,AuxProf).

mi1(Goal,AuxProf) :- predicate_property(Goal,built_in), functor(Goal, Pred, Arity),
	     entry_update(Pred,Arity,AuxProf), ! ,Goal.

mi1(Goal,AuxProf) :-
	     \+ predicate_property(Goal,built_in),
	     functor(Goal, Pred, Arity),
	     entry_update(Pred,Arity,AuxProf),
	     clause(Goal,Body),
	     mi1(Body,AuxProf).


profiler(Call,Profile) :- Counter = counter([]), mi1(Call,Counter),!, makeProfiler(Counter,Profile).

entry_update(Pred, Arity, Counter):-  arg(1,Counter,L), delete_member([Pred,Arity,Cont],L,LR), NewCont is Cont+1, nb_setarg(1,Counter,[[Pred,Arity,NewCont]|LR]), !.
entry_update(Pred, Arity, Counter):-  arg(1,Counter,L), nb_setarg(1,Counter,[[Pred,Arity,1]|L]).

%entry_update(Pred, Arity, Counter, Pos):-  arg(1,Counter,L), delete_member_pos(Pos,[Pred,Arity,Cont],L,LR), NewCont is Cont+1, nb_setarg(1,Counter,[[Pred,Arity,NewCont]|LR]), !.
%entry_update(Pred, Arity, Counter, Pos):-  NewPos is Pos+1, entry_update(Pred,Arity,Counter,NewPos), !.
%entry_update(Pred, Arity, Counter, Pos):-  arg(1,Counter,L), not(nth(Pos,L,_)),  nb_setarg(1,Counter,[[Pred,Arity,1]|L]).


makeProfiler(counter([]),[]).
makeProfiler(counter([[Pred,Arity,Cont]|R]),[[Pred/Arity,Cont]|RA]) :- makeProfiler(counter(R),RA).
