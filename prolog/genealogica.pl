genitor(joana,joao).
genitor(joao,jose).
genitor(joao,maria).
genitor(jose,ana).
genitor(jose,paulo).
genitor(maria,pedro).

casad_s(joana, carlos).
casad_s(carlos, joana).

casad_s(joao, julia).
casad_s(julia, joao).

casad_s(jose, mariana).
casad_s(mariana, jose).

irm(X,Y) :- genitor(Z,X), genitor(Z,Y), \=(X,Y). 
prim(X,Y) :- genitor(P1,X), genitor(P2,Y), irm(P1,P2).
net(X,Y) :- genitor(P,X), genitor(Y,P).

descendente(X,Y) :- genitor(Y,X).
descendente(X,Y) :- genitor(P,X), descendente(P,Y).

solteir_(X) :- genitor(X, _), \+ casad_s(X, _).

filh_unic_(X) :-
    genitor(_P, X),
    \+ (irm(X, Y), X \= Y).
