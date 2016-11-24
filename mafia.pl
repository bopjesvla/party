% status

:- thread_local voting/5,
 action_history/5,
 access/4,
 current_phase/1, % false during signups
 player/2, % user_id, player_id
 setup_roles/1,
 phase_timer/1,
 speed/1.

try(X) :- X,!.
try(_).

:- op(995, fx, try).

setup_roles([[t, [cop]], [t, []], [m, []]]).
setup_phases(string_chars("dn")).
setup_size(N) :- setup_roles(S), length(S, N).

current_phase_name(P) :- current_phase(PhaseNumber), setup_phases(Phases), length(Phases, L), I = PhaseNumber mod L, nth0(I, Phases, P).
player_count(N) :- findall(P, player(_, P), Players), length(Players, N).

remove_phase_timer :- try (retract(phase_timer(T)), remove_alarm(T)).
set_phase_timer(T) :- speed(Speed), alarm(T, next_phase, Id), asserta(phase_timer(Id)).

join(User) :- player(User, _),!.
join(User) :- \+ current_phase(X), setup_size(S), player_count(P), P < S, asserta(player(User, User)), try (player_count(P2), P2 = S, start_game_countdown).

start_game_countdown :- set_phase_timer(10).

unvote(Player, Channel, Action) :- current_phase(P), can_unvote(P, Player, Channel, Action), try retract(voting(P, Player, Channel, Action, _)).
vote(Player, Channel, Action, Targets) :- current_phase(P), unvote(Player, Channel, Action), can_vote(Player, Channel, Action, Targets), asserta(voting(P, Player, Channel, Action, Targets)).

can_unvote(Player, Channel, Action) :- access(Player, Channel, _, now), channel_action(Channel, Action), action_target(Player, Action, false), \+ locked(Channel, Action).
can_vote(Player, Channel, Action, Targets) :- access(Player, Channel, _, now), channel_action(Channel, Action), action_target(Player, Action, Targets), \+ locked(Channel, Action).

role_action([cop], check, _).
role_action([doc], protect, _).
role_action([{shot, 1} | Role], Action, Channel) :- action_history(_, Action, Channel, _), role_action(Role, Action).

blocked(Player, (_, Phase, _), _) :- status(Phase, Player, blocked).
blocked(Player, Action, Targets) :- member(X, Targets), status(Phase, X, rolestopped).

do_action(Player, Action, Targets) :- action(Player, Action, Targets), \+ blocked(Player).
do_action(Player, Action, Targets).

status(Phase, Player, blocked) :- action(_, (block, _), Targets), member(Player, Targets).

:- begin_tests(mafia).

% assert a sequence of actions and facts
seq(Clauses) :- forall(member(C, Clauses), assertion(C)).
:- op(1150, fx, seq).

test(speed) :- assert(speed(1)). % run phase timers at x10000
test(setup) :- assert(setup_roles([[t, [cop]], [t, []], [m, []]])).
test(join) :- seq [ player_count(0), join(1), player_count(1), join(5), player_count(2), join(5), player_count(2)].

test(starting) :- seq [ \+ phase_timer(_), join(3), phase_timer(_), \+ join(2), join(3), \+ current_phase(_), sleep(0.0001), current_phase(1)].

:- end_tests(mafia).
