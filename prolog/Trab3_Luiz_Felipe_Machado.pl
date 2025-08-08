/* Family facts */
pai(joao, maria).
pai(pedro, andre).
pai(andre, jorge).
pai(emerson, claudio).
mae(ana, pedro).
mae(maria, emerson).

/* Citizenship facts */
cidadania(joao, alema).
cidadania(pedro, italiana).
cidadania(ana, portuguesa).
cidadania(joao, britanica).

/* Parent and ancestor rules */
parent(P, C) :- pai(P, C).
parent(P, C) :- mae(P, C).

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, X), ancestor(X, D).

/* Citizenship rules */

% Base case: A person has the citizenship directly
tem_direito(Person, Cidadania) :-
    cidadania(Person, Cidadania).

% Italy: No generation limit
tem_direito(Person, italia) :-
    ancestor(Ancestor, Person),
    cidadania(Ancestor, italiana).

% Portugal: Up to grandchildren
tem_direito(Person, portuguesa) :-
    parent(Parent, Person),
    cidadania(Parent, portuguesa).

tem_direito(Person, portuguesa) :-
    parent(Parent, Person),
    parent(Grandparent, Parent),
    cidadania(Grandparent, portuguesa).

% UK: Direct parent
tem_direito(Person, britanica) :-
    parent(Parent, Person),
    cidadania(Parent, britanica).

% Germany: Up to grandchildren
tem_direito(Person, alema) :-
    parent(Parent, Person),
    cidadania(Parent, alema).

tem_direito(Person, alema) :-
    parent(Parent, Person),
    parent(Grandparent, Parent),
    cidadania(Grandparent, alema).
