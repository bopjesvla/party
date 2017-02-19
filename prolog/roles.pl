role_action(R, A, T, C) :- role_action(R, A, T, C, [], _).
role_action(R, A, T, C, O) :- role_action(R, A, T, C, [], O).

role_action(([], Role), Action, Targets, Channel, ActionMods, ActionMods) :-
  main_role_action(Role, Action), !,
  (member(phase, ActionMods), !; default_phase_constraint(Role)),
  (member(target, ActionMods), !; default_target_constraint(Role, Channel, Targets)).

role_action(([], Alias), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  alias(Alias, Role),
  role_action(Role, Action, Targets, Channel, LeftActionMods, RightActionMods).

% special limiting modifiers, mostly ones that alter default behavior such as day and self
role_action(([Mod | Mods], Role), Action, Targets, Channel, LeftActionMods, RightActionMods) :-
  \+ mod_excludes(Mod, Action, Targets, Channel),
  action_mods(Mod, LeftActionMods, NewActionMods),
  role_action((Mods, Role), Action, Targets, Channel, NewActionMods, RightActionMods).

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

alias(bulletproof, (["strong-willed", "compulsive", "self"], doctor)).

mod_excludes([Xchar | "-shot"], Action, _, Channel) :-
  X is Xchar - 48,
  X > 0,
  X < 10,
  count(action_history(_, action(_, Action, _, Channel), _), Count),
  Count >= X.

mod_excludes("self", _, Targets, Channel) :-
  member(Target, Targets),
  other(Target, Channel).

mod_excludes("day", _, _, _) :-
  \+ current_phase_name(day).

action_mods("self", N, [target | N]) :- !.
action_mods("day", N, [phase | N]) :- !.

% add any remaining role mods as an action mod; will be ignored if useless
action_mods(Mod, N, [Mod | N]).
