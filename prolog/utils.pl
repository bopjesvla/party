erl(Code, Result) :-
  ecall(erlog_demo:efunc(Code), Result).

random_permutation(List, Shuffled) :-
  erl('Elixir.Enum':shuffle(List), Shuffled).

uid(Random) :-
  erl(crypto:strong_rand_bytes(8), Bytes),
  erl(base64:encode(Bytes), Random).

% test a sequence of actions and facts
%% seq(Clauses) :- forall(member(C, Clauses), assertion(C)).
%% :- op(995, fx, seq).

ignore(X) :- X,!.
ignore(_).

count(Clause, N) :- findall(Clause, Clause, X), length(X, N).

get_time(Time) :-
  erl(os:timestamp(millisecond), R),
  Time is T * 0.001.

get_time(Goal, Duration) :-
  get_time(Before),
  ( Goal -> true ; true ),
  get_time(After),
  Duration is After - Before.

forall(Cond, Action) :- \+ (Cond, \+ Action).

% only for predicates without side effects
retract_all(Clause) :- retract(Clause), call(Clause), !, retract_all(Clause).
retract_all(_).

nth0(0, [X | _], X) :- !.
nth0(N, [_ | L], X) :- O is N - 1, nth0(O, L, X).

nth1(1, [X | _], X) :- !.
nth1(N, [_ | L], X) :- O is N - 1, nth1(O, L, X).

select(Selected, [Selected | Y], Y).
select(Selected, [X | Y], [X | YminusOne]) :- select(Selected, Y, YminusOne).

run_tests(Failures) :-
  findall([test(Name), failure_at(FailureStatement)], (
    clause(test(Name), Body),
    locate_failure(Body, FailureStatement)
  ), Failures).

locate_failure((Statement, Body), Failure) :-
  Statement, !,
  locate_failure(Body, Failure).

locate_failure((FailureStatement, _), FailureStatement) :- !.

locate_failure(FailureStatement, FailureStatement) :- \+ FailureStatement.
