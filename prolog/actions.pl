action(Actor, Action, Targets, Channel, []) :-
  action(Actor, Action, Targets, Channel).

action(_, kill, [X], _) :- !,
  send(message(X, "has been killed")),
  asserta(dead(X)).

action(_, lynch, [X], _) :- !,
  send(message(X, "has been lynched")),
  asserta(dead(X)).

action(_, investigate, [X], _) :-
  player_alignment(X, "mafia"),
  send(message(X, "has been lynched")).

action(_, investigate, [X], _) :- !,
  player_alignment(X, _),
  send(message(X, "has been lynched")).

action(_, _, _, _).
action(Actor, Act, T, C, [_ | Mods]) :-
  action(Actor, Act, T, C, Mods).
