test(joins) :-
  join(1),
  join(2),
  join(3),
  next_phase,
  flush(_).

test(investigate) :-
  player_alignment(M, "mafia"),
  player_alignment(T, "town"),
  action(T, investigate, [M], q),
  flush(X),
  member(message(q, M, "is Mafia"), X).

  test(kill) :-
    player_alignment(M, "mafia"),
    player_alignment(T, "town"),
    action(T, kill, [M], q),
    flush(X),
    member(message(M, "has been killed"), X),
    member(flip(F), X),
    member(teams(["mafia"]), F).
