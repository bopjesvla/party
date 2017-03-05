test(next_phase) :- next_phase.

test(investigate) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, investigate, [M], q),
  flush(X),
  member(message(q, M, "is Mafia"), X).

test(trackerlike) :-
  resolve_and_process_actions([
    action(-601, track, [-602], tracker_channel, []),
    action(-602, watch, [-601], watcher_channel, []),
    action(-601, peep, [-602], voyeur_channel, []),
    action(-602, follow, [-601], follower_channel, []),
    action(-603, visit, [-601], visitor_channel, []),
    action(-603, visit, [-602], visitor_channel, [ninja])
  ]),
  flush(X),
  member(message(tracker_channel, -601, "was targeted"), X).

test(kill) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, kill, [M], q),
  flush(X),
  member(message(M, "has been killed"), X),
  member(flip(M, F), X),
  dead(M),
  member(teams(["mafia"]), F).
