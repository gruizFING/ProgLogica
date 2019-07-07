hay_camino(a, b).
hay_camino(c, b).
hay_camino(X, Y) :- hay_camino(Y, X).
hay_camino(X, Z) :- hay_camino(X, Y), hay_camino(Y, Z).
