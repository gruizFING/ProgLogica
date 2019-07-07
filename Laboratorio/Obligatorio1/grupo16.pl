%
% Predicados simples sobre listas
%

% nth(+Pos,?L,?E)
% E es el elemento en la posición Pos (contando a partir de 1) de la lista L.

nth(1,[X|_],X).
nth(P,[_|R],X) :- P > 1, Q is P-1, nth(Q,R,X).

%choose(+N,?L,?Sub)
%Sub es una subsecuencia de largo N de la lista L.

choose(0,_,[]).
choose(N,[C|R],[C|R2]):- N>0, I is N-1, largo(R2,I), choose(I,R,R2).
choose(N,[_|R],R2):- N>0, largo(R2,N), choose(N,R,R2).

largo([],0).
largo([_|L],N):- N>0, X is N-1, largo(L,X).


%remove_alter(?L,?R)
%R es la lista producto de borrar alternadamente la lista L, comenzando por el primero.

append([], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

remove_alter([],[]).
remove_alter([_],[]).
remove_alter([_,I|J],R) :- append([I],K,R), remove_alter(J,K).

% insert_in_order(+X,+L,?R)
% R es el resultado de insertar ordenadamente el elemento X en la lista
% ordenada L

insert_in_order(X,[],[X]).
insert_in_order(X,[C|R],[X,C|R]) :- X =< C.
insert_in_order(X,[C|R],[C|Res]) :- X > C, insert_in_order(X, R, Res).

%
%Predicados avanzados sobre listas
%

% nth_with_merge(+Pos,+L1,+L2,?E)
% E es el elemento en la posición Pos de la lista producto de combinar
% ordenadamente las listas ordenadas L1 y L2.

nth_with_merge(P,L,[],E) :- nth(P,L,E).
nth_with_merge(P,[],L,E) :- L \= [], nth(P,L,E).
nth_with_merge(1,[X|_],[Y|_],E) :- min(X,Y,E).
nth_with_merge(P,[C1|R1],[C2|R2],E) :- C1 < C2 , Q is P-1, nth_with_merge(Q,R1,[C2|R2],E).
nth_with_merge(P,[C1|R1],[C2|R2],E) :- C1 >= C2, Q is P-1, nth_with_merge(Q,[C1|R1],R2,E).

% min(+X,+Y,?Min)
% X e Y son comparables
% Min es el minimo entre X e Y

min(X,Y,X) :- X =< Y.
min(X,Y,Y) :- Y < X.


% member_sorted(+X,+L)
% X es un elemento de la lista ordenada L, no se debe recorrer la lista innecesariamente.

member_sorted(X,[X|_]).
member_sorted(X,[C|R]) :- C < X, member_sorted(X,R).


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

% insertion_sort(+L,?S)
% S es la lista ordenada de L utilizando el algoritmo de insertion sort:
% obtener cada elemento de la lista L e insertarlo ordenadamente en una
% nueva lista.

insertion_sort(L,S) :- ins_sort_acc(L,[],S).

% ins_sort_acc(+L,?Acc,?S)
% L es la lista a ordernar
% Acc es donde se va guardando la lista parcialmente ordenada
% S es la lista ordenada

ins_sort_acc([],S,S).
ins_sort_acc([C|R],Acc,S) :- insert_in_order(C,Acc,NAcc), ins_sort_acc(R,NAcc,S).

% quicksort(+L,?S)
% S es la lista ordenada de L utilizando el algoritmo quicksort.

% función auxiliar para quicksort
% Se generan dos listas, L1 con los elementos menores a K, y L2 con
% los elementos mayores a K.

colocar(_,[],[],[]).
colocar(K,[C|R],[C|L1],L2) :- K >= C, colocar(K,R,L1,L2).
colocar(K,[C|R],L1,[C|L2]) :- K <  C, colocar(K,R,L1,L2).

% Una vez que se tienen los elementos mayores a C en una lista y
% los elementos menores a C en otra, se hace quicksort de ambas.
% luego se las juntas.
% Notar que el pivoteo se realiza en el primer elemento.

quicksort([],[]).
quicksort([C|R],S) :- colocar(C,R,L1,L2), quicksort(L1,S1),quicksort(L2,S2), append(S1,[C|S2],S).


%
% Predicados sobre Matrices
%

% matrix(+X,+Y,?M)
% M es una matriz de X filas e Y columnas.
% La matriz se representa mediante una lista de X filas, donde cada fila
% es una lista de Y celdas.

matrix(1,Y,[F|[]]) :- Y > 0, largo(F,Y).
matrix(X,Y,[F|RF]) :- X > 1, Y > 0, largo(F,Y), I is X-1, matrix(I,Y,RF).

% cell_at(+X,+Y,?M,?P)
% P es la celda en la fila X y columna Y de la matriz M, las filas y
% columnas comienzan en 1.

cell_at(1,Y,[C|_],P) :- nth(Y,C,P).
cell_at(X,Y,[_|R],P) :- X > 1, Y > 0, I is X-1, cell_at(I,Y,R,P).

% select_column(?M,?C,?MRest)
% C es la primera columna de la matriz M, MRest es la matriz M sin su
% primera columna.

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
