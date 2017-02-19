test(start_game) :- next_phase.

test(simple) :-
  channel_type(Channel, player_role),
  role_action(([], village), lynch, _, Channel).

test(xshot) :-
  channel_type(Channel, player_role),
  role_action((["1-shot"], village), lynch, _, Channel).

test(self) :-
  channel_type(Channel, player_role),
  access(Player, Channel),
  \+ role_action(([], village), lynch, Player, Channel),
  role_action((["self"], village), lynch, Player, Channel).

test(day) :-
  channel_type(Channel, player_role),
  \+ role_action(([], doctor), protect, _, Channel),
  role_action((["day"], doctor), protect, _, Channel).

test(weak) :-
  channel_type(Channel, player_role),
  role_action((["weak"], village), lynch, _, Channel, [], ActionMods),
  member("weak", ActionMods).

test(compulsive) :-
  channel_type(Channel, player_role),
  role_action(([], village), lynch, [noone], Channel),
  \+ role_action((["compulsive"], village), lynch, [noone], Channel).
  
