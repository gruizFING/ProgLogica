/*-------- Latin square --------*/

% latin_square_sat(+N,?M)
latin_square_sat(N,M) :- N>1, codificarEntrada(N), ejecutarMiniSat, decodificarSalida(N,M), !.

codificarEntrada(N) :- tell('latin.in'),
		       NVars is N ^ 3, NClaus is N ^ 4,
		       writef("p cnf %w %w\n", [NVars, NClaus]),
		       writeEspecificacion(1,N,1,NVars), !,
		       Nf is N-1, Nfsig is Nf-1, NVars2 is N*N,
		       writeEspecificacion(1,N,1,1,Nf,Nfsig,2,NVars), !,
		       writeEspecificacion(2,N,1,1,Nf,Nfsig,2,NVars2), !,
		       told.

writeEspecificacion(1,_,NVars,NVars) :- writef("%w 0\n",[NVars]).
writeEspecificacion(1,N,X,NVars) :- X mod N =:= 0, writef("%w 0\n",[X]),
                                    Xn is X+1, writeEspecificacion(1,N,Xn,NVars).
writeEspecificacion(1,N,X,NVars) :- writef("%w ",[X]),
                                    Xn is X+1, writeEspecificacion(1,N,Xn,NVars).


getXSig(Xsig,N,Xsign) :- Xsig mod N =:= 0, Xsign is (Xsig + N*(N-1) + 1).
getXSig(Xsig,_,Xsign) :- Xsign is Xsig+1.


writeEspecificacion(1,N,X,Y,1,0,Xsig,NVars) :- Xsig =:= NVars+1,
	                                       Yn is Y+N, writef("-%w -%w 0\n",[X,Yn]).

writeEspecificacion(1,N,X,Y,1,0,Xsig,NVars) :- Yn is Y+N, writef("-%w -%w 0\n",[X,Yn]),
					       getXSig(Xsig,N,Xsign), Nf is N-1, Nfsig is Nf-1,
					       writeEspecificacion(1,N,Xsig,Xsig,Nf,Nfsig,Xsign,NVars).

writeEspecificacion(1,N,X,Y,1,Nfsig,Xsig,NVars) :- Yn is Y+N, writef("-%w -%w 0\n",[X,Yn]),
                                                   Xn is X+N, Nfsign is Nfsig-1,
						   writeEspecificacion(1,N,Xn,Xn,Nfsig,Nfsign,Xsig,NVars).

writeEspecificacion(1,N,X,Y,Nf,Nfsig,Xsig,NVars) :- Yn is Y+N, writef("-%w -%w 0\n",[X,Yn]),
                                                    Nfn is Nf-1, writeEspecificacion(1,N,X,Yn,Nfn,Nfsig,Xsig,NVars).

writeEspecificacion(2,N,X,Y,1,0,Xsig,NVars) :- Xsig =:= NVars+1,
	                                       Yn is Y+(N*N), writef("-%w -%w 0\n",[X,Yn]).

writeEspecificacion(2,N,X,Y,1,0,Xsig,NVars) :- Yn is Y+(N*N), writef("-%w -%w 0\n",[X,Yn]),
					       Xsign is Xsig+1, Nf is N-1, Nfsig is Nf-1,
					       writeEspecificacion(2,N,Xsig,Xsig,Nf,Nfsig,Xsign,NVars).

writeEspecificacion(2,N,X,Y,1,Ncsig,Xsig,NVars) :- Yn is Y+(N*N), writef("-%w -%w 0\n",[X,Yn]),
                                                   Xn is X+(N*N), Ncsign is Ncsig-1,
						   writeEspecificacion(2,N,Xn,Xn,Ncsig,Ncsign,Xsig,NVars).

writeEspecificacion(2,N,X,Y,Nc,Ncsig,Xsig,NVars) :- Yn is Y+(N*N), writef("-%w -%w 0\n",[X,Yn]),
                                                    Ncn is Nc-1, writeEspecificacion(2,N,X,Yn,Ncn,Ncsig,Xsig,NVars).



ejecutarMiniSat :- shell('minisat_static.exe latin.in latin.out',_).


decodificarSalida(N,M) :- see('latin.out'),
                          get_char('S'), skip(10),
			  decodificarSalida(N,1,1,[],[],M),
			  seen.

decodificarSalida(_,1,1,[],S,S) :- peek_char('0').

decodificarSalida(N,V,P,F,S,M) :- peek_char('-'), skip(32),
	                          Vn is V+1,
				  decodificarSalida(N,Vn,P,F,S,M).

decodificarSalida(N,V,N,F,S,M) :- not(peek_char('-')),
	                          NEsp is N-V, saltearEspacios(NEsp),
				  append(F,[V],Fn), append(S,[Fn],Sn),
				  decodificarSalida(N,1,1,[],Sn,M).

decodificarSalida(N,V,P,F,S,M) :- not(peek_char('-')),
	                          NEsp is N-V, saltearEspacios(NEsp),
				  append(F,[V],Fn),
				  Pn is P+1,
				  decodificarSalida(N,1,Pn,Fn,S,M).


saltearEspacios(0) :- skip(32).
saltearEspacios(N) :- N>0, skip(32), Nn is N-1, saltearEspacios(Nn).



% latin_square_sat_many(+N,+HowMany,?Bag)
latin_square_sat_many(N,HowMany,Bag) :- N>1, HowMany>0,
	                                latin_square_sat(N,M), HowManyRest is HowMany-1,
	                                latin_square_sat_many_acc(N,HowManyRest,[M],Bag), !.

latin_square_sat_many_acc(_,0,Bag,Bag).

latin_square_sat_many_acc(N,HowMany,BagAcc,Bag) :- addClausulaSolucionEntrada, ejecutarMiniSat, decodificarSalida(N,M),
	                                           HowManyRest is HowMany-1, append(BagAcc,[M],BagAccN),
						   latin_square_sat_many_acc(N,HowManyRest,BagAccN,Bag).


addClausulaSolucionEntrada :- open("latin.out",read,InpStream),
			      get_char(InpStream,'S'), skip(InpStream,10),
			      open("latin.in",append,OutStream),
			      addClausula(1,InpStream,OutStream),
			      close(InpStream), close(OutStream).

addClausula(_,InpStream,OutStream) :- peek_char(InpStream,'0'),
	                              put_char(OutStream,'0'), nl(OutStream).

addClausula(Var,InpStream,OutStream) :- peek_char(InpStream,'-'), skip(InpStream,32),
	                                write(OutStream,Var), put_code(OutStream,32),
				        Varn is Var+1, addClausula(Varn,InpStream,OutStream).

addClausula(Var,InpStream,OutStream) :- not(peek_char(InpStream,'-')), skip(InpStream,32),
	                                put_char(OutStream,'-'), write(OutStream,Var), put_code(OutStream,32),
				        Varn is Var+1, addClausula(Varn,InpStream,OutStream).





/*---------- N Values ----------*/

% Extraimos algunas ideas de http://ai.ia.agh.edu.pl/wiki/prolog:pllib:findall
% Goal es el Objetivo  que debe comumplirse en este laboratorio lo probaremos con el latin_square(N,M)
% Template es el termino que queremos tomar los valores que que cumplen con el Goal en el caso de este laboratorio lo probaremos con M
% K es cuantas veces queremos que se cumpla el objetivo
% Bag es la lista con los valores de Template para los cuales se cumplio el objetivo
% Precondicion se asume que K es mayor a 0, tambien se asume que existe esa K cantidad de soluciones
% n_values(+Goal,+Template,+K,-Bag)
% La idea es usar una variable global de contadora haciendo backtracking hasta que la variable sea 0
% n_values(+Goal,+Template,+K,-Bag)

n_values(Goal,Template,K,Bag):- nb_setval(cont,K),not(busca_sol(Goal,Template)),recolectar(Bag).

busca_sol(Goal,Template):-	call(Goal),% Busco una solucion
							assertz(queue(Template)), % la agrego a la cola de la base de datos de swi prolog que no es alterada por el backtraking
							dec(N),% Decremento el contador
							N is 0,assertz(queue(bottom)),!, % si el contador es 0 tengo la cantidad de soluciones que queria paro el backtraking agregando botton luego cortando con !
							fail. % debo fallar aproposito para que no me instancie template con el valor del paso N
							
% dec(-P)
% En P cargo el valor actual luego de decrementaro de la variable global cont
dec(P):- nb_getval(cont ,N),succ(P,N),nb_setval(cont,P). %obtengo el valor del contador y  le resto al contador 1

%codigo de recolectar basado en el mencionado al principio
% En la lista L cargo los valores que estan en cola de la base de datos de swi prolog 
% recolectar(-L)
recolectar(L)  :-
  retract( queue(X) ), !,              % saco una solucion de la cola
  ( X == bottom, !, L = []             % me fijon si queda alguna mas sino quedan X es bottom retonando en N la lista vacia
    ;
    L = [X|R], recolectar(R)).  % como X no es bottom agrego X y sigo agregando las que queden
	

	
% Los predicados a continuacion son del primer laboratorio y solo se usan para calcular el latin_square

% select_column(?M,?C,?MRest)
% C es la primera columna de la matriz M, MRest es la matriz M sin su
% primera columna.

% permutation(+X,?Y)
% La lista Y es una permutación de los elementos de la lista X.

permutation([],[]).
permutation(X1,[C|R]) :- delete_member(C,X1,X2), permutation(X2,R).

% delete_member(+E,+L,?R)
% E es elemento a borrar de la lista L
% E debe ser miembro de L
% R es la lista resultado

delete_member(E,[E|R],R).
delete_member(E,[X|R],[X|Res]) :- delete_member(E,R,Res).
select_column([[C|Rf]|[]],[C|[]],[Rf]).
select_column([[Ccol|Rfila]|Matriz],[Ccol|Col],[Rfila|Rmatriz]):-select_column(Matriz,Col,Rmatriz).


% latin_square(+N,?Resul)
% La matriz Resul representa un cuadrado latino de orden N.
% Un cuadrado latino de orden N es una matriz compuesta enteramente por enteros entre 1 y N, y
% cuyas filas y columnas no contienen elementos repetidos.
% Idea: Verificar que cada fila sea una permutacion de la otra y que
% sean permutaciones de la lista [N...3,2,1] , luego ver que cada
% columna generada sea permutacion de la lista [N...3,2,1]

% permRows(+L,?M,+N)
% L es la lista que se permuta para ir formando las filas de la matriz
% M, N*N es la dimension de la matriz generada.
% Este predicado es el generador de los posibles cuadrados
% latinos (soluciones).

permRows(L,[C|[]],1) :- permutation(L,C).
permRows(L,[C|R],N1) :- N1 > 1, permutation(L,C), N2 is N1-1, permRows(L,R,N2).

% permCols(+L,+M,+N)
% Se verifica que cada columna de la Matriz M sea una permutacion
% de la Lista L, N se usa para saber cuando terminar de recorrer.
% Este predicado es para checkear soluciones, no es para generar.

permCols(L,M,1) :- select_column(M,Col,_), permutation(L,Col).
permCols(L,M,N) :- select_column(M,Col,R), permutation(L,Col), I is N-1, permCols(L,R,I).

% listaN(+N,?L).
% L lista de largo N con la forma [N...3,2,1]

listaN(1,[1]).
listaN(N,[N|R]):-succ(C,N),listaN(C,R).

% Primero se usa permRows para generar posibles matrizes soluciones y
% luego se checkean con permCols.

latin_square(N,Resul) :- listaN(N,L), permRows(L,Resul,N), permCols(L,Resul,N).


