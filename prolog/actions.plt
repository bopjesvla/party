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
    action(-603, protect, [-602], doctor_channel, []),
    action(-603, investigate, [-602], cop_channel, ["ninja"])
  ]),
  flush(X),
  member(message(tracker_channel, -601, "was targeted"), X),
  member(message(watcher_channel, -603, "targeted"), X),
  member(message(voyeur_channel, -602, protect), X),
  member(message(follower_channel, -601, peep), X),
  member(message(follower_channel, -601, track), X),
  \+ member(message(_, _, investigate), X).

test(history) :-
  resolve_and_process_actions([
    action(-701, block, [-702], blocker_channel, ["instant"])
  ]),
  resolve_and_process_actions([
    action(-702, investigate, [-701], cop_channel, [])
  ]),
  flush(X),
  X = [].

test(kill) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, kill, [M], q),
  flush(X),
  member(message(M, "has been killed"), X),
  member(flip(M, F), X),
  dead(M),
  member(teams(["mafia"]), F).
