% seq/1, status

:- module(mafia, [join/1, vote/4, unvote/3, join_channel/2, access/4, set_setup/1, game_info/2]).

:- use_module(library(http/json)).

setup_size(N) :- findall(Id, setup_alignment(Id, _), Ids), length(Ids, N).

:- thread_local
   voting/5,
   action_history/3,
   access/4,
   current_phase/1, % false during signups
   signups_end_time/1,
   game_end_time/1,
   player/2, % user_id, player_id
   phase_timer/1,
   speed/1,
   message/1,
   setup_alignment/2,
   player_role/2,
   alignment_role/2,
   global_role/1,
   channel_role/2,
   channel_type/2,
   locked/3,
   dead/1,
   setup_phases/1.

:- include(roles).
:- include(actions).
:- include(resolve).
:- include(utils).

:- begin_tests(set_setup).

test(speed) :- set(speed(10)).
test(alignments) :- set(setup_alignment(1,t)), set(setup_alignment(2,t)), set(setup_alignment(3,m)). % run phase timers at x10000
test(global_role) :- set(global_role(([], village))).
test(alignment_roles) :- set(alignment_role(m, ([], killer))).
test(player_role) :- set(player_role(1, ([], cop))).

test(set_setup) :- set_setup(m{'teams': [], 'player_roles': [], 'alignment_roles': [], 'global_roles': [], phases: [day, night]}).

:- end_tests(set_setup).

set_setup(Setup) :-
  forall(member(A, Setup.teams), assertz(setup_alignment(A.player, A.team))),
  forall(member(A, Setup.player_roles), assertz(player_role(A.player, (A.mods, A.role)))),
  forall(member(A, Setup.alignment_roles), assertz(alignment_role(A.team, (A.mods, A.role)))),
  forall(member(A, Setup.global_roles), assertz(global_role((A.mods, A.role)))),
  assertz(setup_phases(Setup.phases)).

uid(X) :- random_between(17_000_000, 260_000_000, R), format(atom(X), '~16r', [R]).

send(X) :- assertz(message(X)).
flush(Res) :- findall(Msg, message(Msg), Res), retractall(message(_)).
state(X) :- asserta(X), send(X).

phase_name(PhaseNumber, Name) :-
  setup_phases(Phases),
  length(Phases, L),
  I is PhaseNumber mod L,
  nth0(I, Phases, Name).

current_phase_name(Name) :-
  current_phase(P),
  phase_name(P, Name).

players(Players) :- findall(P, player(_, P), Players).
player_count(N) :- players(Players), length(Players, N).
full_game :- player_count(P), setup_size(S), P >= S.

alive(X) :- player(X, _), \+ dead(X).

remove_phase_timer :- retract(phase_timer(T)), remove_alarm(T),!.
remove_phase_timer.

%set_phase_timer(T) :-
%speed(Speed),
%RealT is T / Speed,
%alarm(RealT, next_phase, Id),
%asserta(phase_timer(Id)).

game_info(User, m{active: Active, access: Access}) :-
  player(User, Player),
  findall(m{channel: C, members: Members, actions: Actions, votes: Votes}, (
      join_channel(User, C),
      findall(Member, join_channel(C, Member), Members),
      findall(m{act: Action, opt: Targets}, channel_action(C, Action, Targets), Actions),
      ignore(current_phase(P)),
      findall(m{player: Player, action: Action, targets: T}, voting(P, Player, C, Action, T), Votes)
  ), Active),
  findall(m{channel: C, start: S, end: E}, access(C, Player, S, E), Access).

join(User) :- player(User, _), !.
join(User) :-
  \+ current_phase(_),
  \+ full_game,
  asserta(player(User, User)), % player id is the id of the first user
  (full_game, start_game_countdown, !; true).

start_game_countdown :- send(next_phase(10)).

:- begin_tests(signups).

:- [game1].

%test(set_setup) :- assert(setup_roles([[t, [cop]], [t, []], [m, []]])).
test(join) :-
  seq [player_count(0),
   join(1),
   player_count(1),
   join(5),
   player_count(2),
   join(5),
   player_count(2)].

test(signups_game_info, [X = _{active: [_{channel: pre, members: _, actions: [], votes: []}], access: _}]) :-
  game_info(1, X).

test(starting, [X = []]) :-
  flush(X),
  join(3),
  \+ join(2),
  join(3).

test(game_full_message, [X = [next_phase(10)]]) :-
  flush(X).

:- end_tests(signups).

next_phase :-
  full_game,
  end_phase,
  increase_current_phase,
  start_phase.

locked_actions(Actions) :-
  current_phase(P),
  findall(action(Actor, Action, Targets, Channel), (
    locked(Channel, Action, Targets),
    once(voting(P, Actor, Channel, Action, Targets); Actor = noone)
    ), Actions).

start_phase.

end_phase :-
  current_phase(_), !, % game has already started
  locked_actions(Actions),
  resolve(Actions, SuccessfulActions),
  process_actions(SuccessfulActions).

end_phase :- start_game. % ending signups = starting the game

increase_current_phase :- retract(current_phase(P)), Next is P + 1, asserta(current_phase(Next)), !.
increase_current_phase :- asserta(current_phase(0)).

start_game :-
  players(Players),
  random_permutation(Players, ShuffledPlayers),
  forall(player_role(N, Role), (
  nth0(N, ShuffledPlayers, Player),
  create_channel(indie, Role, Channel),
         grant_access(Player, Channel)
     )),
    forall(alignment_role(Alignment, Role), ( % for every alignment role, add a channel
         create_channel(alignment, Role, Channel),
  setup_alignment(N, Alignment),
  forall(nth0(N, ShuffledPlayers, Player), grant_access(Player, Channel))
     )),
    forall(global_role(Role), (
         create_channel(global, Role, Channel),
         asserta(global_channel(Channel)),
         forall(member(Player, Players), grant_access(Player, Channel))
     )).

channel_action(Channel, Action, Targets) :-
  channel_role(Channel, Role),
  role_action(Role, Action, Targets, Channel),
  current_phase(P),
  \+ action_history(P, action(_, Action, _, Channel), _).

:- begin_tests(game_start).

test(start) :- next_phase.
test(global_channel) :- seq [
          channel_role(Channel, ([], village)),
          access(1, Channel, _, now),
          channel_action(Channel, lynch, [3])
      ].
test(role_channel) :- seq [ channel_role(Channel, ([], cop)),
          access(_, Channel, _, now)
          % channel_action(Channel, investigate, [noone])
        ].

:- end_tests(game_start).

create_channel(Type, Role, Channel) :-
  uid(Channel),
  asserta(channel_role(Channel, Role)),
  state(channel_type(Channel, Type)).

grant_access(Player, Channel) :- access(Player, Channel, _, now), !.
grant_access(Player, Channel) :-
  get_time(T),
  asserta(access(Player, Channel, T, now)).

kick(Player, Channel) :-
  retract(access(Player, Channel, Start, now)),
  get_time(T),
  asserta(access(Player, Channel, Start, T)).

join_channel(User, pre) :-
  player(User, Player),
  \+ current_phase(_).

join_channel(User, Channel) :-
  player(User, Player),
  access(Player, Channel, _, now),
  channel_action(Channel, _, _).

unvote(Player, Channel, Action) :-
  current_phase(P),
  can_unvote(Player, Channel, Action),
  (retractall(voting(P, Player, Channel, Action, _)); true).

vote(Player, Channel, Action, Targets) :-
  current_phase(P),
  can_vote(Player, Channel, Action, Targets),
  (retract(voting(P, Player, Channel, Action, _)); true),
  asserta(voting(P, Player, Channel, Action, Targets)),
  check_hammer(Channel, Action, Targets).

can_unvote(_Player, Channel, Action) :-
  channel_action(Channel, Action, _),
  \+ locked(Channel, Action, _).

can_vote(_Player, Channel, Action, Targets) :-
  channel_action(Channel, Action, Targets),
  \+ locked(Channel, Action, _).

check_hammer(Channel, Action, Targets) :-
  aggregate_all(count, access(_, Channel, _, now), ChannelMemberCount),
  current_phase(P),
  aggregate_all(count, voting(P, _, Channel, Action, Targets), VoteCount),
  VoteCount > ChannelMemberCount / 2, !,
  lock(Channel, Action, Targets),
  maybe_end_phase.

check_hammer(_, _, _).

lock(Channel, Action, Targets) :-
    asserta(locked(Channel, Action, Targets)).

:- begin_tests(voting).

test(voting) :- seq [
      channel_type(Channel, global),
      vote(1, Channel, lynch, [3]),
      \+ vote(1, Channel, lynch, [1235]),
      \+ vote(3, Channel, kill, [1]),
      vote(3, Channel, lynch, [1]),
      unvote(1, Channel, _),
      vote(5, Channel, lynch, [5])
    ].

:- end_tests(voting).

maybe_end_phase :-
    forall(channel_action(Channel, Action, _), locked(Channel, Action, _)), !,
    end_phase.

maybe_end_phase.

:- begin_tests(end_phase).

test(lynch) :- seq [
       channel_type(Channel, global),
       vote(5, Channel, lynch, [1]),
       \+ vote(5, Channel, lynch, [1])
    ].

test(lynch_logged, all(X = [action(5, lynch, [1], Channel)])) :-
    channel_type(Channel, global),
    action_history(0, X, success).

:- end_tests(end_phase).

%role_action([cop], check, _).
%role_action([doc], protect, _).
%role_action([{shot, 1} | Role], Action, Channel) :- action_history(_, Action, Channel, _), role_action(Role, Action).

%blocked(Player, (_, Phase, _), _) :- status(Phase, Player, blocked).
%blocked(Player, Action, Targets) :- member(X, Targets), status(Phase, X, rolestopped).

%do_action(Player, Action, Targets) :- action(Player, Action, Targets), \+ blocked(Player).
%do_action(Player, Action, Targets).

%status(Phase, Player, blocked) :- action(_, (block, _), Targets), member(Player, Targets).
