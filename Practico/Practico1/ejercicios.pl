% Ej 1
% X es el progenitor(madre o padre) de Y
progenitor(X,Y).
% X e Y son diferentes
distintos1(X,Y).
% X e Y estan casados
casados(X,Y).

% X e Y son hermanos
hermano(X,Y) :- distintos1(X,Y), progenitor(Z,X), progenitor(Z,Y).
% X es tio(a) de Y
tio(X,Y) :- progenitor(Z,Y), hermano(Z,X).
% X es tio(a) politico(a) de Y
tio_politico(X,Y) :- tio(Z,Y), casados(Z,X).
% X e Y son cuñados
cuñado(X,Y) :- hermano(X,Z1), casados(Y,Z1)
               ;
               hermano(Y,Z2), casados(X,Z2).
% X e Y son concuñados
concuñado(X,Y) :- casados(X,Z1), cuñado(Z1,Z2), casados(Z2,Y)
                  ;
                  casados(Y,Z3), cuñado(Z3,Z4), casados(Z4,X).
% X es suegro(a) de Y
suegro(X,Y) :- casados(Y,Z), progenitor(X,Z).
% X e Y son consuegros
consuegro(X,Y) :- progenitor(X,Z1), suegro(Y,Z1)
		  ;
                  progenitor(Y,Z2), suegro(X,Z2).

% Ej2
nat(0).
nat(X) :- X>0, Xn is X-1, nat(Xn).

suma(X,Y,S) :- nat(X), nat(Y), S is X+Y.
resta(X,Y,R) :- nat(X), nat(Y), R is X-Y.
producto(X,Y,P) :- nat(X), nat(Y), P is X*Y.
distintos(X,Y) :- nat(X), nat(Y), X =\= Y.
mayor(X,Y) :- nat(X), nat(Y), X > Y.

% Recursivo puro, re ineficiente. Con accum mucho mejor...
factorial(0,1).
factorial(1,1).
factorial(X,Y) :- resta(X,1,X2), factorial(X2,Y2), producto(X,Y2,Y).

potencia(X,0,1) :- nat(X).
potencia(X,1,X) :- nat(X).
potencia(X,Y,Z) :- resta(Y,1,Y2), potencia(X,Y2,Z2), producto(X,Z2,Z).


% Ej3
% recursivo
largo1(L,N) :- largoRec(L,N).
largoRec([],0).
largoRec([_|S],N) :- largoRec(S,Nn), N is Nn+1.

% con accum
largo2(L,N) :- largoAccum(L,0,N).
largoAccum([],Accum,Accum).
largoAccum([_|S],Accum,N) :- AccumN is Accum+1, largoAccum(S,AccumN,N).

ultimo([C|[]],C).
ultimo([_|R],X) :- ultimo(R,X).

sin_ultimo([_|[]],[]).
sin_ultimo([C|R1],[C|R2]) :- sin_ultimo(R1,R2).

% Ej4

sumaLista([],0).
sumaLista([C|R],N) :- sumaLista(R,X), N is X+C.



