/*-------- Latin square --------*/


% latin_square_sat(+N,?M)
latin_square_sat(N,M) :- codificarEntrada(N), not(ejecutarMiniSat), decodificarSalida(N,M), !.

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



ejecutarMiniSat :- shell('minisat_static.exe latin.in latin.out').


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
latin_square_sat_many(N,HowMany,Bag).

/*---------- N Values ----------*/

% n_values(+Goal,+Template,+K,-Bag)
n_values(Goal,Template,Times,Bag).
