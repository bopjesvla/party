% seq/1, status

:- module(mafia, [join/1, vote/4, unvote/3, join_channel/2]).

:- use_module(utils).

setup_size(N) :- findall(Id, setup_alignment(Id, _), Ids), length(Ids, N).

:- thread_local
voting/5,
action_history/5,
access/4,
current_phase/1, % false during signups
player/2, % user_id, player_id
setup_roles/1,
phase_timer/1,
speed/1,
message/2.

uid(X) :- random_between(17_000_000, 260_000_000, R), format(atom(X), '~16r', [R]).

send(Type, Msg) :- asserta(message(Type, Msg)).

:- op(995, fx, try).

current_phase_name(P) :-
	current_phase(PhaseNumber),
	setup_phases(Phases),
	length(Phases, L),
	I = PhaseNumber mod L,
	nth0(I, Phases, P).

players(Players) :- findall(P, player(_, P), Players).
player_count(N) :- players(Players), length(Players, N).
full_game :- player_count(P), setup_size(S), P >= S.

remove_phase_timer :- try (retract(phase_timer(T)), remove_alarm(T)).
set_phase_timer(T) :-
	speed(Speed),
	RealT is T / Speed,
	alarm(RealT, next_phase, Id),
	asserta(phase_timer(Id)).

join(User) :- player(User, _), !.
join(User) :-
	\+ current_phase(_),
	\+ full_game,
	asserta(player(User, User)),
	try (full_game, start_game_countdown).

start_game_countdown :- set_phase_timer(10).

:- begin_tests(signups).

:- [game1].

test(speed) :- seq [ set(speed(10)) ]. % run phase timers at x10000
%test(setup) :- assert(setup_roles([[t, [cop]], [t, []], [m, []]])).
test(join) :-
	seq [player_count(0),
	join(1),
	player_count(1),
	join(5),
	player_count(2),
	join(5),
	player_count(2)].

test(starting) :-
	seq [ \+ phase_timer(_),
	join(3),
	phase_timer(_),
	\+ join(2),
	join(3),
	\+ current_phase(_)].

:- end_tests(signups).

next_phase :-
	end_phase,
	increase_current_phase,
	start_phase.

resolve(_).

end_phase :-
	current_phase(P),!,
	resolve(P).

end_phase :- start_game. % ending signups = starting the game

increase_current_phase :- retract(current_phase(P)), Next is P + 1, asserta(current_phase(Next)),!.
increase_current_phase :- asserta(current_phase(0)).

start_game :-
	players(Players),
	random_permutation(Players, ShuffledPlayers),
	forall(setup_role(N, Role), (
		nth0(N, ShuffledPlayers, Player),
		create_channel([Player], Role)
    )),
	forall(alignment_role(Alignment, Role), (
		setup_alignment(N, Alignment),
		findall(Player, nth0(N, ShuffledPlayers, Player), Team),
		create_channel(Team, Role) 
	)).

:- begin_tests(game_start).

test(start) :- seq [sleep(0.2), current_phase(1)].

:- end_tests(game_start).

channel_action(_, _).

create_channel(Players, Role) :-
	uid(Channel),
	forall(member(Players, P), grant_access(P, Channel)),
	asserta(channel_role(Channel, Role)).

grant_access(Player, Channel) :- access(Player, Channel, _, now), !.
grant_access(Player, Channel) :-
	get_time(T),
	asserta(access(Player, Channel, T, now)).

kick(Player, Channel) :-
	retract(access(Player, Channel, Start, now)),
	get_time(T),
	asserta(access(Player, Channel, Start, T)).

join_channel(Player, Channel) :-
	access(Player, Channel, _, now),
	channel_action(Channel, _).

unvote(Player, Channel, Action) :-
	current_phase(P),
	can_unvote(P, Player, Channel, Action),
	try retract(voting(P, Player, Channel, Action, _)).

vote(Player, Channel, Action, Targets) :-
	current_phase(P),
	unvote(Player, Channel, Action),
	can_vote(Player, Channel, Action, Targets),
	asserta(voting(P, Player, Channel, Action, Targets)).

can_unvote(Player, Channel, Action) :-
	channel_action(Channel, Action),
	action_target(Player, Action, false),
	\+ locked(Channel, Action).

can_vote(Player, Channel, Action, Targets) :-
	channel_action(Channel, Action),
	action_target(Player, Action, Targets),
	\+ locked(Channel, Action).

%role_action([cop], check, _).
%role_action([doc], protect, _).
%role_action([{shot, 1} | Role], Action, Channel) :- action_history(_, Action, Channel, _), role_action(Role, Action).

%blocked(Player, (_, Phase, _), _) :- status(Phase, Player, blocked).
%blocked(Player, Action, Targets) :- member(X, Targets), status(Phase, X, rolestopped).

%do_action(Player, Action, Targets) :- action(Player, Action, Targets), \+ blocked(Player).
%do_action(Player, Action, Targets).

%status(Phase, Player, blocked) :- action(_, (block, _), Targets), member(Player, Targets).
