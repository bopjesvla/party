:- module(utils, [try/1, seq/1, set/1, unset/1, op(1150, fx, seq)]).

% tests need local assert/retract
set(X) :- asserta(X).
unset(X) :- retract(X).

% test a sequence of actions and facts
seq(Clauses) :- forall(member(C, Clauses), assertion(C)).

try(X) :- X,!.
try(_).
