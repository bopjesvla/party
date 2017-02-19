test(start) :-
  next_phase,
  current_phase(0),
  current_phase_name(day).

test(access_messages) :-
  flush(X),
  member(leave(_), X),
  member(create_channel(_), X),
  member(join(_, _), X).

test(global_channel) :-
  channel_role(Channel, ([], village)),
  channel_action(Channel, X, _),
  X = lynch,
  channel_type(Channel, global_role),
  join_channel(1, Channel),
  channel_action(Channel, lynch, [3]).

test(role_channel) :-
  channel_role(Channel, ([], cop)),
  access(_, Channel)
  .

test(team_channel) :-
  channel_role(Channel, ([], killer)),
  channel_type(Channel, team_role),
  access(_, Channel).

test(player_channel) :-
  channel_type(Channel, player),
  access(_, Channel),
  channel_role(Channel, nil).

:- end_tests(game_start).
:- begin_tests(voting).

test(voting) :- channel_type(Channel, global_role),
  vote(1, Channel, lynch, [3]),
  \+ vote(1, Channel, lynch, [1235]),
  \+ vote(_, _, kill, [1]),
  vote(3, Channel, lynch, [1]),
  unvote(1, Channel, _),
  vote(5, Channel, lynch, [5]),
  flush(X),
  X = [vote(1, Channel, lynch, [3]),
  vote(3, Channel, lynch, [1]),
  unvote(1, Channel, lynch),
  vote(5, Channel, lynch, [5])].

:- end_tests(voting).
:- begin_tests(end_phase).

test(lynch) :-
  channel_type(Channel, global_role),
  player_team(P, "town"), % :(
  unvote(3, Channel, _),
  vote(5, Channel, lynch, [P]),
  vote(1, Channel, lynch, [P]),
  vote(7, Channel, lynch, [P]), % if this fails, next_phase probably failed
  flush(X),
  member(message(P, "has been lynched"), X).

test(lynch_logged) :-
  channel_type(Channel, global_role),
  findall(X, action_history(0, X, success), Y),
  Y = [action(7, lynch, [_], Channel, [])].

test(night) :-
  current_phase(1),
  current_phase_name(night),
  \+ vote(5, Channel, lynch, [1]),
  \+ vote(P, _, kill, [P]),
  channel_action(_, investigate, [noone]),
  ignore(vote(_, _, investigate, [noone])),
  vote(_, _, kill, _).

test(kill_logged) :-
  channel_type(Channel, team_role),
  findall(X, action_history(1, X, success), Y),
  member(action(_, kill, [_], Channel, []), Y).

test(end) :-
  flush(X),
  member(end_game([_]), X).
