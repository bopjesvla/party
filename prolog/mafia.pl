% seq/1, status

%% :- module(mafia, [join/1, vote/4, unvote/3, join_channel/2, access/2, setup_game/1, game_info/2, flush/1, create_channel/3, next_phase/0, call_self/1]).

setup_size(N) :- findall(Id, setup_team(Id, _), Ids), length(Ids, N).

% defining dynamic predicates the erlog way
voting(q, q, q, q, q) :- fail.
action_history(q, q, q) :- fail.
access(q, q) :- fail.
current_phase(q) :- fail.
player(q, q) :- fail. % GameSlot.id, GameSlot.player
player_team(q, q) :- fail.
phase_timer(q, q) :- fail.
speed(q) :- fail.
setup_team(q, q) :- fail.
setup_role(q, q, q) :- fail.
channel_role(q, q) :- fail.
channel_type(q, q) :- fail.
locked(q, q, q, q) :- fail.
dead(q) :- fail.
setup_phases(q) :- fail.
player_won(q) :- fail.

:- include(roles).
:- include(actions).
:- include(resolve).
:- include(utils).

signups :- \+ current_phase(_).

state(X) :- asserta(X), send(X).
player(P) :- player(P, _).

phase_name(Phase, Name) :-
  setup_phases(Phases),
  length(Phases, L),
  I is Phase mod L,
  nth0(I, Phases, Name).

phase_number(Phase, Number) :-
  setup_phases(Phases),
  length(Phases, L),
  Number is truncate(Phase / L + 1).

current_phase_name(Name) :-
  current_phase(P),
  phase_name(P, Name).

current_phase_number(Number) :-
  current_phase(P),
  phase_number(P, Number).

current_phase_info([name(Name), number(Number), next(End)]) :-
  nil_fallback(End, phase_timer(_, End)),
  nil_fallback(Name, current_phase_name(Name)),
  nil_fallback(Number, current_phase_number(Number)).

players(Players) :- findall(P, player(P), Players).

alive(X) :- player(X), \+ dead(X).

game_info(Player, [active(Active), inactive(Inactive), player_status(Players), phase(PhaseInfo), teams(Teams)]) :-
  player(Player),
  findall([channel(C), members(Members), actions(Actions), votes(Votes), role(Role), type(Type)], (
      join_channel(Player, C),
      findall(Member, join_channel(Member, C), Members),
      nil_fallback(Role, channel_role(C, Role)),
      channel_type(C, Type),
      findall([act(Action), opt(Targets)], channel_action(C, Action, Targets), Actions),
      current_phase(P),
      findall([player(Player), action(Action), targets(T)], voting(P, Player, C, Action, T), Votes)
  ), Active),
  findall([channel(C), members(Members), actions(Actions), votes(Votes), role(Role), type(Type)], (
      access(Player, C),
      \+ join_channel(Player, C),
      findall(Member, join_channel(Member, C), Members),
      nil_fallback(Role, channel_role(C, Role)),
      channel_type(C, Type)
  ), Inactive),
  current_phase_info(PhaseInfo),
  findall([slot(P), status(Status)], (
      player(P), status(P, Status)
  ), Players),
  findall(T, player_team(Player, T), Teams).

set_phase_timer(After) :-
  remove_phase_timer,
  speed(Speed),
  Ms is truncate(After * 1000 / Speed),
  get_time(Time),
  End is Time + Ms,
  erl(erlang:self, Self),
  erl(erlang:send_after(Ms, Self, do_next_phase), Timer),
  send(next_phase(End)), !,
  asserta(phase_timer(Timer, End)).

remove_phase_timer :-
  phase_timer(Timer, _), !,
  erl(timer:cancel(Timer), _),
  retract(phase_timer(_, _)).

remove_phase_timer.

next_phase :-
  end_phase,
  end_game_or_next_phase.

end_game_or_next_phase :-
  soft_end_game, !.

end_game_or_next_phase :-
  increase_current_phase,
  start_phase,
  forall((
    channel_role(Channel, _),
    \+ join_channel(_, Channel)),
    send(leave(Channel))
  ),
  maybe_next_phase.

locked_actions(Actions) :-
  current_phase(P),
  findall(action(Actor, Action, Targets, Channel, ActionMods), (
    locked(Channel, Action, Targets, ActionMods),
    % since votes are added using asserta, this is the last vote
    once(voting(P, Actor, Channel, Action, Targets)),
    \+ member(noone, Targets)
    ), Actions).

end_phase :-
  current_phase(_), !, % game has already started
  locked_actions(Actions),
  retract_all(locked(_, _, _, _)),
  resolve(Actions, SuccessfulActions),
  % shuffle to prevent reading into result order
  random_permutation(SuccessfulActions, Shuffled),
  forall(member(A, Shuffled), call(A)),
  ignore(soft_end_game).

end_phase :- start_game. % ending signups = starting the game

increase_current_phase :- retract(current_phase(P)), Next is P + 1, asserta(current_phase(Next)), !.
increase_current_phase :- asserta(current_phase(0)).

start_phase :- !.

start_game :-
  players(Players),
  forall(player(Player), (
    create_channel(player, nil, Channel),
    grant_access(Player, Channel)
  )),
  forall(setup_role(player, N, Role), (
    player(Player, N),
    create_channel(player_role, Role, Channel),
    grant_access(Player, Channel)
   )),
  forall((setup_team(N, Team), player(Player, N)), (
    asserta(player_team(Player, Team))
  )),
  forall(setup_role(team, Team, Role), ( % for every team role, add a channel
    create_channel(team_role, Role, Channel),
    forall(player_team(P, Team), grant_access(P, Channel))
  )),
  forall(setup_role(global, _, Role), (
    create_channel(global_role, Role, Channel),
    asserta(global_channel(Channel)),
    forall(member(Player, Players), grant_access(Player, Channel))
  )).


channel_action(C, A, T) :- channel_action(C, A, T, _).
channel_action(C, A) :- once(channel_action(C, A, _, _)).
channel_action(C) :- once(channel_action(C, _, _, _)).

channel_action(Channel, Action, Targets, ActionMods) :-
  channel_role(Channel, Role),
  role_action(Role, Action, Targets, Channel, [], ActionMods),
  current_phase(P),
  \+ action_history(P, action(_, Action, _, Channel), _).

create_channel(Type, Role, Channel) :-
  uid(Channel),
  send(create_channel(Channel)),
  asserta(channel_role(Channel, Role)),
  asserta(channel_type(Channel, Type)).

grant_access(Player, Channel) :- access(Player, Channel), !.
grant_access(Player, Channel) :-
  player(Player),
  send(join(Player, Channel)),
  asserta(access(Player, Channel)).

retract_access(Player, Channel) :- \+ access(Player, Channel), !.
retract_access(Player, Channel) :-
  retract(access(Player, Channel)),
  send(leave(Player, Channel)),
  asserta(access(Player, Channel)).

join_channel(Player, Channel) :-
  access(Player, Channel),
  (channel_type(Channel, player); once(channel_action(Channel, _, _))).

unvote(Player, Channel, Action) :-
  player(Player),
  current_phase(P),
  can_unvote(Player, Channel, Action),
  ignore(retract_all(voting(P, Player, Channel, Action, _))),
  send(unvote(Player, Channel, Action)).

vote(Player, Channel, Action, Targets) :-
  player(Player),
  can_vote(Player, Channel, Action, Targets, ActionMods),
  do_vote(Player, Channel, Action, Targets, ActionMods).

do_vote(Player, Channel, Action, Targets, ActionMods) :-
  ignore(retract(voting(P, Player, Channel, Action, _))),
  asserta(voting(P, Player, Channel, Action, Targets)),
  send(vote(Player, Channel, Action, Targets)),
  check_hammer(Channel, Action, Targets, ActionMods).

can_unvote(Player, Channel, Action) :-
  access(Player, Channel),
  alive(Player),
  channel_action(Channel, Action, _),
  \+ locked(Channel, Action, _, _).

can_vote(Player, Channel, Action, Targets, ActionMods) :-
  access(Player, Channel),
  alive(Player),
  channel_action(Channel, Action, Targets, ActionMods),
  \+ locked(Channel, Action, _, _).

check_hammer(Channel, Action, Targets, ActionMods) :-
  count(access(_, Channel), ChannelMemberCount),
  current_phase(P),
  count(voting(P, _, Channel, Action, Targets), VoteCount),
  VoteCount > ChannelMemberCount / 2, !,
  lock(Channel, Action, Targets, ActionMods),
  maybe_next_phase.

check_hammer(_, _, _, _).

lock(Channel, Action, Targets, ActionMods) :-
    asserta(locked(Channel, Action, Targets, ActionMods)).

lock_deterministic_actions :-
  forall((
    channel_role(C, _),
    count(can_vote(P, C, A, T, _), 1)
  ), vote(Actor, C, Act, Targets)).

maybe_next_phase :-
  lock_deterministic_actions,
  \+ can_vote(_, _, _, _, _),
  next_phase.

maybe_next_phase.

status(Player, dead) :- dead(Player), !.
status(Player, alive).


flip(Player) :-
  findall(Team, player_team(Player, Team), Teams),
  findall(Role, (
    access(Player, Channel),
    channel_type(Channel, player_role),
    channel_role(Channel, Role)
  ), Roles),
  send(flip(Player, [roles(Roles), teams(Teams)])).
