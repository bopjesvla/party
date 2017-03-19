test(send) :-
    retract_all(mock),
    send(hey),
    asserta(mock).

test(send_mock) :-
  asserta(mock),
  send(hey),
  flush([hey]).

test(nil_fallback) :-
  nil_fallback(X, (X = 1)),
  \+ X = 2,
  X = 1,
  nil_fallback(Y, (Y = 1, Y = 2)),
  \+ Y = 1,
  Y = nil.

test(dict) :-
  dict(X).

test(find_dicts) :-
  find_dicts([value(X)], member(X, [1]), Out).
