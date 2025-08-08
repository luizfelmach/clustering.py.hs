ama(vincent, mia).
ama(marcelo, mia).

temCiumes(X,Y) :- dif(X,Y), ama(X,Z), ama(Y,Z).
emCiumes(X) :- temCiumes(X,_); temCiumes(_,X).


fatorial(0,1).
fatorial(N, F) :- N > 0, N1 is N-1, fatorial(N1, F1), F is N * F1.
