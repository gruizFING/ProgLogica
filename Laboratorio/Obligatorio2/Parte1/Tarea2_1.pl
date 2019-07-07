
nth(1,[X|_],X).
nth(P,[_|R],X) :- P > 1, Q is P-1, nth(Q,R,X).

cell_at(1,Y,[C|_],P) :- nth(Y,C,P).
cell_at(X,Y,[_|R],P) :- X > 1, Y > 0, I is X-1, cell_at(I,Y,R,P).

match(C,X,Ficha,0) :- nth(X,Ficha,C).
match(C,X,[F1,F2,F3,F4,_],R) :- R>0, Rn is R-1, match(C,X,[F4,F1,F2,F3,_],Rn).

addPosiciones([[C1,C2,C3,C4]],P,[[C1,C2,C3,C4,P]]).
addPosiciones([[C1,C2,C3,C4]|R1],P,[[C1,C2,C3,C4,P]|R2]) :- Pn is P+1, addPosiciones(R1,Pn,R2).

sacarFicha([[_,_,_,_,P]|R],P,R).
sacarFicha([C|R],P,[C|F]) :- sacarFicha(R,P,F).


solve_puzzle(File,N,Solution) :- open(File,read, FichasStream), read(FichasStream, FichasMatrix), close(FichasStream),
	                         addPosiciones(FichasMatrix,1,Fichas),
				 solve_puzzle_matrix(Fichas, Fichas, N, [1,1], [], Sol),
				 reverse(Sol,Solution).


solve_puzzle_matrix(Fichas, FichasO, N, [N,N], SolAc, Sol) :- get_ficha(8, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
						              append([[N,N,Pos,Rot]],SolAc,Sol).


solve_puzzle_matrix(Fichas, FichasO, N, [1,1], SolAc, Sol) :- get_ficha(1, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[1,1,Pos,Rot]],SolAc,SolAcN),
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [1,2], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [1,C], SolAc, Sol) :- C>1, C<N,
							      get_ficha(2, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
							      sacarFicha(Fichas,Pos,FichasN),
							      append([[1,C,Pos,Rot]],SolAc,SolAcN),
	                                                      Cn is C+1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [1,Cn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [1,N], SolAc, Sol) :- get_ficha(3, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[1,N,Pos,Rot]],SolAc,SolAcN),
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [2,1], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,1], SolAc, Sol) :- F>1, F<N,
	                                                      get_ficha(4, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
							      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,1,Pos,Rot]],SolAc,SolAcN),
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [F,2], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,N], SolAc, Sol) :- F>1, F<N,
	                                                      get_ficha(5, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,N,Pos,Rot]],SolAc,SolAcN),
	                                                      Fn is F+1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [Fn,1], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [N,1], SolAc, Sol) :- get_ficha(6, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[N,1,Pos,Rot]],SolAc,SolAcN),
	                                                      solve_puzzle_matrix(FichasN, FichasO, N, [N,2], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [N,C], SolAc, Sol) :- C>1, C<N,
	                                                      get_ficha(7, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[N,C,Pos,Rot]],SolAc,SolAcN),
							      Cn is C+1,
							      solve_puzzle_matrix(FichasN, FichasO, N, [N,Cn], SolAcN, Sol).

solve_puzzle_matrix(Fichas, FichasO, N, [F,C], SolAc, Sol) :- F>1, F<N, C>1, C<N,
	                                                      get_ficha(9, Fichas, FichasO, 0, SolAc, Pos, Rot, N),
	                                                      sacarFicha(Fichas,Pos,FichasN),
						              append([[F,C,Pos,Rot]],SolAc,SolAcN),
	                                                      Cn is C+1,
						              solve_puzzle_matrix(FichasN, FichasO, N, [F,Cn], SolAcN, Sol).




get_ficha(1,[[0,_,_,0,P]|_],_,R,_,P,R,_).


get_ficha(2,[[0,_,_,C4,Pos]|_],FichasO,Rot,[[1,_,PF,RF]|_],Pos,Rot,_) :- nth(PF,FichasO,Ficha),
								        match(C4,2,Ficha,RF).

get_ficha(3,[[0,0,_,C4,Pos]|_],FichasO,Rot,[[1,_,PF,RF]|_],Pos,Rot,_) :- nth(PF,FichasO,Ficha),
									match(C4,2,Ficha,RF).

get_ficha(4,[[C1,_,_,0,Pos]|_],FichasO,Rot,Sol,Pos,Rot,N) :- cell_at(N,3,Sol,PFicha), cell_at(N,4,Sol,RFicha),
							    nth(PFicha,FichasO,Ficha),
							    match(C1,3,Ficha,RFicha).

get_ficha(5,[[C1,0,_,C4,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									    match(C4,2,Ficha1,RF1),
									    Nn is N-1,
									    cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									    nth(PF2,FichasO,Ficha2),
									    match(C1,3,Ficha2,RF2).

get_ficha(6,[[C1,_,0,0,Pos]|_],FichasO,Rot,Sol,Pos,Rot,N) :-  cell_at(N,3,Sol,PFicha), cell_at(N,4,Sol,RFicha),
							     nth(PFicha,FichasO,Ficha),
							     match(C1,3,Ficha,RFicha).

get_ficha(7,[[C1,_,0,C4,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									    match(C4,2,Ficha1,RF1),
									    Nn is N-1,
									    cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									    nth(PF2,FichasO,Ficha2),
									    match(C1,3,Ficha2,RF2).

get_ficha(8,[[C1,0,0,C4,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									    match(C4,2,Ficha1,RF1),
									    Nn is N-1,
									    cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									    nth(PF2,FichasO,Ficha2),
									    match(C1,3,Ficha2,RF2).

get_ficha(9,[[C1,_,_,C4,Pos]|_],FichasO,Rot,[[_,_,PF1,RF1]|RS],Pos,Rot,N) :- nth(PF1,FichasO,Ficha1),
									    match(C4,2,Ficha1,RF1),
									    Nn is N-1,
									    cell_at(Nn,3,RS,PF2), cell_at(Nn,4,RS,RF2),
									    nth(PF2,FichasO,Ficha2),
									    match(C1,3,Ficha2,RF2).


get_ficha(C,[[C1,C2,C3,C4,P]|RestoFichas],FichasO,Rot,Sol,PosF,RotF,N) :- Rot<3, Rnuevo is Rot+1,
							  get_ficha(C,[[C4,C1,C2,C3,P]|RestoFichas],FichasO,Rnuevo,Sol,PosF,RotF,N).

get_ficha(C,[_|RestoFichas],FichasO,3,Sol,PosF,RotF,N) :- get_ficha(C,RestoFichas,FichasO,0,Sol,PosF,RotF,N).











