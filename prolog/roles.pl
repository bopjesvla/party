role_action(R, A, T, C) :- role_action(R, A, T, C, [], _).
role_action(R, A, T, C, O) :- role_action(R, A, T, C, [], O).

role_action(([], Role), Action, Targets, Channel, ActionMods, ActionMods) :-
  main_role_action(Role, Action),
  (member(phase, ActionMods), !; default_phase_constraint(Role)),
  (member(target, ActionMods), !; default_target_constraint(Role, Channel, Targets)).

% special limiting modifiers, mostly ones that alter default behavior such as day and self
role_action(([Mod | Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  \+ role_action_filter(Mod, Action, Targets, Channel),
  action_mods(Mod, LeftActionMods, NewActionMods),
  role_action(([Mods], Role), Action, Targets, Channel, NewActionMods, RightActionMods).

default_phase_constraint(village) :-
  !, current_phase_name(day).

default_phase_constraint(_) :-
  current_phase_name(night).

alive_or_noone(Player) :- alive(Player).
alive_or_noone(noone).

default_target_constraint(_, Channel, [Target]) :-
  alive_or_noone(Target),
  (channel_type(Channel, global_role); other(Target, Channel)).

other(Player, Channel) :- \+ access(Player, Channel).

main_role_action(village, lynch).
main_role_action(cop, investigate).
main_role_action(killer, kill).
main_role_action(doctor, protect).
main_role_action(roleblocker, block).

role_action_filter(XShot, Action, _, Channel) :-
  append(X, "-shot", XShot),
  erl('Elixir.String':to_integer(X), N),
  count(action_history(_, action(_, Action, _, Channel), _), Count),
  Count >= N.

role_action_filter("self", _, Targets, Channel, Ci) :-
  member(Target, Targets),
  other(Target, Channel),
  Target \= noone.

role_action_filter("day", _, Targets, Channel, Ci, [target | Ci]) :-
  forall(member(Target, Targets), access(Target, Channel)).

action_mod("self", N, [target | N]) :- !.
action_mod("day", N, [phase | N]) :- !.

% add any remaining role mods as an action mod; will be ignored if useless
action_mod(Mod, N, [Mod | N]).
