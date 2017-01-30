erl(Code, Result) :-
  ecall(erlog_demo:efunc(Code), Result).

dict_get_ex(Key, Dict, Value) :-
  erl(maps:get(Key, Dict), Value).

% test a sequence of actions and facts
%% seq(Clauses) :- forall(member(C, Clauses), assertion(C)).
%% :- op(995, fx, seq).

try(X) :- X,!.
try(_).

cpu_time(Time) :-
  ecall(erlog_demo:efunc(erlang:statistics(runtime)), R),
  R = [T|_],
  Time is T * 0.001.

cpu_time(Goal, Duration) :-
  cpu_time(Before),
  ( Goal -> true ; true ),
  cpu_time(After),
  Duration is After - Before.

forall(Cond, Action) :- \+ ( Cond, \+ Action).

% only for predicates without side effects
retract_all(Clause) :- retract(Clause), call(Clause), !, retractall(Clause).
retract_all(_).

%% run_tests(Errors) :-
%%   findall([Name, ErrorStatement], (
%%     clause(test(Name), Body),
%%     locate_error(Body, ErrorStatement)
%%   ), Errors).

run_tests(Errors) :-
  findall([Name, ErrorStatement], (
    clause(test(Name), Body),
    locate_error(Body, ErrorStatement)
  ), Errors).

locate_error((Statement, Body), Error) :-
  call(Statement), !,
  locate_error(Body, Error). 

locate_error((ErrorStatement, _), ErrorStatement) :- !.

locate_error(ErrorStatement, ErrorStatement) :- \+ call(ErrorStatement).
