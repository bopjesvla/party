role_action(R, A, T, C) :- role_action(R, A, T, C, []).

role_action(([], Role), Action, [Target], Channel, Constraints) :-
    main_role_action(Role, Action),
    (member(phase, Constraints); default_phase_constraint(Role)),
    (member(target, Constraints); default_target_constraint(Role, Channel, Target)).

% role_status(([], bulletproof), bulletproof).

default_phase_constraint(village) :-
    current_phase_name(day), !.

default_phase_constraint(_) :-
    current_phase_name(night).

default_target_constraint(_, Channel, Target) :-
    (alive(Target); Target = noone),
    (channel_type(Channel, global); other(Channel, Target)).

other(Channel, Player) :- \+ access(Player, Channel).

main_role_action(village, lynch).
main_role_action(cop, investigate).
main_role_action(killer, kill).
main_role_action(doctor, protect).
main_role_action(roleblocker, block).

:- begin_tests(roles).

:- end_tests(roles).
