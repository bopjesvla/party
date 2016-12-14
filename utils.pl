% tests need local assert/retract
set(X) :- asserta(X).
unset(X) :- retract(X).

% test a sequence of actions and facts
seq(Clauses) :- forall(member(C, Clauses), assertion(C)).
:- op(1150, fx, seq).

try(X) :- X,!.
try(_).
