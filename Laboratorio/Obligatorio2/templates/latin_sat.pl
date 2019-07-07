/*-------- Latin square --------*/

% latin_square_sat(+N,?M)
latin_square_sat(N,M).

% latin_square_sat_many(+N,+HowMany,?Bag)
latin_square_sat_many(N,HowMany,Bag).

/*---------- N Values ----------*/

% n_values(+Goal,+Template,+K,-Bag)
n_values(Goal,Template,Times,Bag).
