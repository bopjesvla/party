test(next_phase) :- next_phase.

test(investigate) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, investigate, [M], q),
  flush(X),
  member(message(q, M, "is Mafia"), X).

test(kill) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, kill, [M], q),
  flush(X),
  member(message(M, "has been killed"), X),
  member(flip(F), X),
  dead(M),
  member(teams(["mafia"]), F).
