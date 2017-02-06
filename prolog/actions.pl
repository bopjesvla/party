action(Actor, Action, Targets, Channel, []) :-
  action(Actor, Action, Targets, Channel).

action(_, kill, [X], _) :- !,
  send(message(X, "has been killed")),
  asserta(dead(X)).

action(_, lynch, [X], _) :- !,
  send(message(X, "has been lynched")),
  asserta(dead(X)).

action(Channel, investigate, [X], _) :-
  player_alignment(X, "mafia"), !,
  send(message(Channel, X, "is Mafia")).
action(Channel, investigate, [X], _) :- !,
  send(message(Channel, X, "is not Mafia")).

action(_, _, _, _).
action(Actor, Act, T, C, [_ | Mods]) :-
  action(Actor, Act, T, C, Mods).
