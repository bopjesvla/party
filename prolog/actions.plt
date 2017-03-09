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
    action(-603, investigate, [-602], ninja_channel, ["ninja"])
  ]),
  flush(X),
  member(message(tracker_channel, -601, "was targeted"), X),
  member(message(watcher_channel, -603, "targeted"), X),
  member(message(voyeur_channel, -602, protect), X),
  member(message(follower_channel, -601, peep), X),
  member(message(follower_channel, -601, track), X),
  \+ member(message(_, _, investigate), X).

test(viral) :-
  Role = (["viral"], visitor),
  asserta(channel_role(viral_channel, Role)),
  resolve_and_process_actions([
    action(-613, visit, [-612], viral_channel, ["viral"])
  ]),
  flush(X),
  access(-612, C),
  channel_role(C, Role).

test(history) :-
  resolve_and_process_actions([
    action(-701, block, [-702], blocker_channel, ["instant"])
  ]),
  resolve_and_process_actions([
    action(-702, investigate, [-701], cop_channel, [])
  ]),
  flush(X),
  X = [].

test(public) :-
  resolve_and_process_actions([
    action(-801, visit, [-802], public_channel, ["public"])
  ]),
  flush(X),
  X = [message(-801, "performed an action")].

test(failed_action_mods) :-
  resolve_and_process_actions([
    action(-801, kill, [-802], public_killer_channel, ["public"]),
    action(-803, protect, [-802], public_doctor_channel, [])
  ]),
  flush(X),
  X = [message(-801, "performed an action")].

test(kill) :-
  player_team(M, "mafia"),
  player_team(T, "town"),
  action(T, kill, [M], q),
  flush(X),
  member(message(M, "has been killed"), X),
  member(flip(M, F), X),
  dead(M),
  member(teams(["mafia"]), F).
