role_action(R, A, T, C) :- role_action(R, A, T, C, [], _).
role_action(R, A, T, C, O) :- role_action(R, A, T, C, [], O).

role_action(([], Role), Action, [Target], Channel, ActionMods, ActionMods) :-
  main_role_action(Role, Action),
  (member(phase, ActionMods), !; default_phase_constraint(Role)),
  (member(target, ActionMods), !; default_target_constraint(Role, Channel, Target)).

% simple limiting modifiers such as 1-shot
role_action(([Mod | Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  role_action_filter(Mod, Action, Targets, Channel), !,
  role_action(([Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods).

% special limiting modifiers, mostly ones that alter default behavior such as day and self
role_action(([Mod | Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  role_action_filter(Mod, Action, Targets, Channel, LeftActionMods, NewActionMods), !,
  role_action(([Mods], Role), Action, Targets, Channel, NewActionMods, RightActionMods).

% special modifiers that apply when actions are processed, e.g. weak, processed in actions.pl
role_action(([Mod | Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  role_action(([Mods], Role), Action, Targets, Channel, [Mod | LeftActionMods], RightActionMods).

default_phase_constraint(village) :-
  !, current_phase_name(day).

default_phase_constraint(_) :-
  current_phase_name(night).

default_target_constraint(_, Channel, Target) :-
  (alive(Target); Target = noone),
  (channel_type(Channel, global_role); other(Channel, Target)).

other(Channel, Player) :- \+ access(Player, Channel).

main_role_action(village, lynch).
main_role_action(cop, investigate).
main_role_action(killer, kill).
main_role_action(doctor, protect).
main_role_action(roleblocker, block).

role_action_filter(XShot, Action, _, Channel) :-
  append(X, "-shot", XShot),
  erl('Elixir.String':to_integer(X), N),
  count(action_history(_, action(_, Action, _, Channel), _), Count),
  Count < N.

role_action_filter("self", _, Targets, Channel, Ci, [target | Ci]) :-
  forall(member(Target, Targets), access(Target, Channel)).

role_action_filter("day", _, Targets, Channel, Ci, [target | Ci]) :-
  forall(member(Target, Targets), access(Target, Channel)).
