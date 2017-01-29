% seq/1, status

:- module(mafia, [join/1, vote/4, unvote/3, join_channel/2, access/2, setup_game/1, game_info/2, flush/1, create_channel/3, next_phase/0, call_self/1]).

:- use_module(library(http/json)).

setup_size(N) :- findall(Id, setup_alignment(Id, _), Ids), length(Ids, N).

:- thread_local
   voting/5,
   action_history/3,
   access/2,
   current_phase/1, % false during signups
   player/2, % user_id, player_id
   phase_timer/2,
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

setup_game(_{setup: Setup, speed: Speed}) :-
  forall(member(A, Setup.teams), assertz(setup_alignment(A.player, A.team))),
  forall(member(A, Setup.player_roles), assertz(player_role(A.player, (A.mods, A.role)))),
  forall(member(A, Setup.alignment_roles), assertz(alignment_role(A.team, (A.mods, A.role)))),
  forall(member(A, Setup.global_roles), assertz(global_role((A.mods, A.role)))),
  asserta(speed(Speed)),
  assertz(setup_phases(Setup.phases)).

:- begin_tests(setup_game).

%% test(speed) :- set(speed(10)).
%% test(alignments) :- set(setup_alignment(1,t)), set(setup_alignment(2,t)), set(setup_alignment(3,m)). % run phase timers at x10000
%% test(global_role) :- set(global_role(([], village))).
%% test(alignment_roles) :- set(alignment_role(m, ([], killer))).
%% test(player_role) :- set(player_role(1, ([], cop))).

test(setup_game) :- setup_game(m{
  setup: m{
    'teams': [m{player: 1, team: t}, m{player: 2, team: t}, m{player: 3, team: m}],
    'player_roles': [m{player: 1, mods: [], role: cop}],
    'alignment_roles': [m{team: m, mods: [], role: killer}],
    'global_roles': [m{mods: [], role: village}],
    phases: [day, night]
  },
  speed: 10
}).

test(signups_channel, [X = [create_channel(Channel)]]) :- create_channel(signups, none, Channel), flush(X).

:- end_tests(setup_game).

signups :- \+ current_phase(_).

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

remove_phase_timer :- retract(phase_timer(T, _)), remove_alarm(T),!.
remove_phase_timer.

%set_phase_timer(T) :-
%speed(Speed),
%RealT is T / Speed,
%alarm(RealT, next_phase, Id),
%asserta(phase_timer(Id)).

game_info(User, m{active: Active}) :-
  player(User, Player),
  findall(m{channel: C, members: Members, actions: Actions, votes: Votes, role: Role, type: Type}, (
      join_channel(User, C),
      findall(Member, join_channel(C, Member), Members),
      channel_role(C, Role),
      channel_type(C, Type),
      findall(m{act: Action, opt: Targets}, channel_action(C, Action, Targets), Actions),
      ignore(current_phase(P)),
      findall(m{player: Player, action: Action, targets: T}, voting(P, Player, C, Action, T), Votes)
  ), Active).

join(User) :- player(User, _), !.
join(User) :-
  signups,
  \+ full_game,
  asserta(player(User, User)), % player id is the id of the first user taking the slot
  (full_game, start_phase_countdown(10), !; true).

call_self(Q) :-
  pengine_self(Self),
  pengine_ask(Self, Q, []).

start_phase_countdown(After) :-
  remove_phase_timer,
  get_time(T),
  speed(Speed),
  End is T + After / Speed,
  % may be subject to race conditions
  alarm_at(End, next_phase, Alarm, [remove(true)]),
  asserta(phase_timer(Alarm, End)),
  send(check_after(After)).

:- begin_tests(signups).

test(join) :-
  seq [player_count(0),
   join(1),
   player_count(1),
   join(5),
   player_count(2),
   join(5),
   player_count(2)].

test(signups_game_info, [X = _{active: [_{channel: _, members: _, actions: [], votes: [], type: signups, role: none}]}]) :-
  game_info(1, X).

test(starting, [X = []]) :-
  flush(X), 
  join(3),
  \+ join(2),
  join(3).

test(game_full_message, [X = [check_after(10)]]) :-
  flush(X),
  remove_phase_timer.

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

end_phase :-
  current_phase(_), !, % game has already started
  locked_actions(Actions),
  resolve(Actions, SuccessfulActions),
  process_actions(SuccessfulActions),
  forall(channel_type(Channel, player_role), (
    \+ join_channel(_, Channel),
    send(leave(all, Channel))
  )),
  forall(channel_type(Channel, alignment), (
    \+ join_channel(_, Channel),
    send(leave(all, Channel))
  )).

end_phase :- send(leave(all, pre)), start_game. % ending signups = starting the game

increase_current_phase :- retract(current_phase(P)), Next is P + 1, asserta(current_phase(Next)), !.
increase_current_phase :- asserta(current_phase(0)).


start_phase :- !, true.
start_phase :-
  forall(player(_, Player), (
    game_info(Player, GameInfo),
    send(game_info(Player, GameInfo))
  )).

start_game :-
  players(Players),
  random_permutation(Players, ShuffledPlayers),
  forall(player(_, Player), (
    create_channel(player, none, Channel),
    grant_access(Player, Channel)
  )),
  forall(player_role(N, Role), (
    nth1(N, ShuffledPlayers, Player),
    create_channel(player_role, Role, Channel),
    grant_access(Player, Channel)
   )),
  forall(alignment_role(Alignment, Role), ( % for every alignment role, add a channel
    create_channel(alignment, Role, Channel),
    forall((setup_alignment(N, Alignment), nth1(N, ShuffledPlayers, Player)), (grant_access(Player, Channel)))
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

test(access_messages, [X = [leave(all, pre), create_channel(_), join(_, _) | _]]) :- flush(X).
test(global_channel) :- seq [
          channel_role(Channel, ([], village)),
          channel_type(Channel, global),
          join_channel(1, Channel),
          channel_action(Channel, lynch, [3])
      ].
test(role_channel) :- seq [ channel_role(Channel, ([], cop)),
          access(_, Channel)
          % channel_action(Channel, investigate, [noone])
        ].

test(alignment_channel) :- seq [ channel_role(Channel, ([], killer)),
          channel_type(Channel, alignment),
          access(_, Channel)
          % channel_action(Channel, investigate, [noone])
        ].

test(player_channel) :- seq [ channel_role(Channel, none),
          channel_type(Channel, player),
          access(1, Channel)
          % channel_action(Channel, investigate, [noone])
        ].

test(start_game_info, [X = _{active: [_{channel: _, members: _, actions: [m{act: lynch, opt: [_]} | _], votes: [], type: global, role: _} | _]}]) :-
  game_info(1, X).
        
:- end_tests(game_start).

create_channel(Type, Role, Channel) :-
  uid(Channel),
  send(create_channel(Channel)),
  asserta(channel_role(Channel, Role)),
  asserta(channel_type(Channel, Type)).

grant_access(Player, Channel) :- access(Player, Channel), !.
grant_access(Player, Channel) :-
  player(User, Player),
  send(join(User, Channel)),  
  asserta(access(Player, Channel)).

retract_access(Player, Channel) :- \+ access(Player, Channel), !.
retract_access(Player, Channel) :-
  retract(access(Player, Channel)),
  get_time(T),
  send(leave(Player, Channel)),
  asserta(access(Player, Channel)).

join_channel(User, Channel) :-
  player(User, Player),
  channel_type(Channel, signups),
  signups.

join_channel(User, Channel) :-
  player(User, Player),
  access(Player, Channel),
  (channel_type(Channel, player); once(channel_action(Channel, _, _))).

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
  aggregate_all(count, access(_, Channel), ChannelMemberCount),
  current_phase(P),
  aggregate_all(count, voting(P, _, Channel, Action, Targets), VoteCount),
  VoteCount > ChannelMemberCount / 2, !,
  lock(Channel, Action, Targets),
  maybe_next_phase.

check_hammer(_, _, _).

lock(Channel, Action, Targets) :-
    asserta(locked(Channel, Action, Targets)).

:- begin_tests(voting).

test(voting) :- channel_type(Channel, global), seq [
      vote(1, Channel, lynch, [3]),
      \+ vote(1, Channel, lynch, [1235]),
      \+ vote(3, Channel, kill, [1]),
      vote(3, Channel, lynch, [1]),
      unvote(1, Channel, _),
      vote(5, Channel, lynch, [5])
    ].

:- end_tests(voting).

maybe_next_phase :-
    forall(channel_action(Channel, Action, _), locked(Channel, Action, _)), !,
    next_phase.

maybe_next_phase.

:- begin_tests(end_phase).

test(lynch) :-
  channel_type(Channel, global),
  seq [
    vote(5, Channel, lynch, [1]),
    \+ vote(5, Channel, lynch, [1]),
    current_phase(1)
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
