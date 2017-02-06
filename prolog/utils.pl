erl(Code, Result2) :-
  ecall(erlog_demo:efunc(Code), Result),
  Result = Result2. % ecall doesn't support bindings :(

erl(Left, _, Right, ToRight) :-
  \+ var(Left),
  erl(ToRight, Right).

erl(Left, ToLeft, Right, _) :-
  \+ var(Right),
  erl(ToLeft, Left).

string_number(String, Number) :-
  erl(Number, 'Elixir.String':to_integer(String), String, erlang:integer_to_list(Number)).

random_permutation(List, Shuffled) :-
  erl('Elixir.Enum':shuffle(List), Shuffled).

uid(Random) :-
  erl(crypto:strong_rand_bytes(8), Bytes),
  erl(base64:encode(Bytes), Random).

ignore(X) :- X,!.
ignore(_).

log(X) :-
  erl('Elixir.IO':inspect(X), _).

bool(X, true) :- X, !.
bool(X, false).

nil_fallback(_, Y) :- Y, !.
nil_fallback(nil, _).

count(Clause, N) :- findall(Clause, Clause, X), length(X, N).

get_time(Time) :-
  erl(os:system_time(millisecond), Time).

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
  asserta(mock),
  findall([test(Name), failure_at(FailureStatement)], (
    clause(test(Name), Body),
    locate_failure(Body, FailureStatement)
  ), Failures), retract_all(mock).

locate_failure((Statement, Body), Failure) :-
  Statement, !,
  locate_failure(Body, Failure).

locate_failure((FailureStatement, _), FailureStatement) :- !.

locate_failure(FailureStatement, FailureStatement) :- \+ FailureStatement.

message(q) :- fail.
mock :- fail.

send(X) :- mock, !, assertz(message(X)).
flush(Res) :- findall(Msg, message(Msg), Res), retract_all(message(_)).

send(Msg) :-
  log(Msg),
  erl(erlang:self, Self),
  erl(erlang:send(Self, Msg), _).


% http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
pivoting(H,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).

quick_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
  pivoting(H,T,L1,L2),
  q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
