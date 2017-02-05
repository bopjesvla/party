:- begin_tests(setup_game).

%% test(setup_game) :- setup_game(m{
%%   setup(m){
%%     'teams'([m){player(1), team(t)}, m{player(2), team(t)}, m{player(3), team(m)}],
%%     'player_roles'([m){player(1), mods([]), role(cop)}],
%%     'alignment_roles'([m){team(m), mods([]), role(killer)}],
%%     'global_roles'([m){mods([]), role(village)}],
%%     phases([day), night]
%%   },
%%   speed(10)
%% }).

test(signups_channel) :-
  create_channel(signups, nil, Channel),
  flush(X),
  X = [create_channel(Channel)].

:- end_tests(setup_game).
:- begin_tests(signups).

test(join) :-
  player_count(0),
  join(1),
  player_count(1),
  join(5),
  player_count(2),
  join(5),
  player_count(2).

test(signups_game_info) :-
  game_info(1, X),
  perm(X, [active([Y]), players(_), next_phase(_)]),
  perm(Y, [channel(_), members(_), actions([]), votes([]), type(signups), role(nil)]).

test(starting) :-
  flush(X),
  X = [join(1, Channel), join(5, Channel)],
  \+ phase_timer(_, _),
  join(3),
  \+ join(2),
  join(3).

test(game_full) :-
  flush(X),
  X = [join(3, Channel), next_phase(_)],
  phase_timer(_, _),
  game_info(1, G),
  member(next_phase(T), G),
  \+ T = nil.

:- end_tests(signups).
:- begin_tests(game_start).

test(start) :-
  next_phase,
  current_phase(0),
  current_phase_name(day).

test(access_messages) :-
  flush(X),
  X = [leave(all, pre), create_channel(_), join(_, _) | _].

test(global_channel) :-
  channel_role(Channel, ([], village)),
  channel_action(Channel, X, _),
  X = lynch,
  channel_type(Channel, global_role),
  join_channel(1, Channel),
  channel_action(Channel, lynch, [3]).

test(role_channel) :- channel_role(Channel, ([], cop)),
  access(_, Channel)
  % channel_action(Channel, investigate, [noone])
  .

test(alignment_channel) :-
          channel_role(Channel, ([], killer)),
          channel_type(Channel, alignment_role),
          access(_, Channel)
          % channel_action(Channel, investigate, [noone])
        .

test(player_channel) :- channel_role(Channel, none),
          channel_type(Channel, player),
          access(1, Channel)
          % channel_action(Channel, investigate, [noone])
        .

%% test(start_game_info, [X = [active([channel(_), members(_), actions([[act(lynch), opt(_)] | _], votes([]), type(global), role(_)] | _)]]) :-
%%   game_info(1, X).
        
:- end_tests(game_start).
:- begin_tests(voting).

test(voting) :- channel_type(Channel, global_role),
      vote(1, Channel, lynch, [3]),
      \+ vote(1, Channel, lynch, [1235]),
      \+ vote(3, Channel, kill, [1]),
      vote(3, Channel, lynch, [1]),
      unvote(1, Channel, _),
      vote(5, Channel, lynch, [5])
    .

:- end_tests(voting).
:- begin_tests(end_phase).

test(lynch) :-
  channel_type(Channel, global_role),
  vote(5, Channel, lynch, [1]).

test(night) :-
  current_phase(1),
  current_phase_name(night),
  \+ vote(5, Channel, lynch, [1]).

test(lynch_logged) :-
    channel_type(Channel, global_role),
    findall(X, action_history(0, X, success), Y),
    Y = [action(5, lynch, [1], Channel, [])].

:- end_tests(end_phase).
