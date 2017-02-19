action(Actor, Action, Targets, Channel, []) :-
  action(Actor, Action, Targets, Channel).

action(_, kill, [X], _) :- !,
  kill(X, "has been killed").

action(_, lynch, [X], _) :- !,
  kill(X, "has been lynched"),
  asserta(dead(X)).

action(_, investigate, [X], Channel) :-
  player_team(X, "mafia"), !,
  send(message(Channel, X, "is Mafia")).
action(_, investigate, [X], Channel) :- !,
  send(message(Channel, X, "is not Mafia")).

action(_, visit, _, _).

action_mod("weak", Actor, _, Targets, _, _) :-
  member(T, Targets),
  player_team(T, Team),
  Team \= "town",
  !, kill(Actor).

action_mod(_,_,_,_,_,_).

action(Actor, Act, T, C, [Mod | Mods]) :-
  action_mod(Mod, Actor, Act, T, C, Mods),
  action(Actor, Act, T, C, Mods).

kill(Player, Message) :-
  send(message(Player, Message)),
  flip(Player, Flip),
  send(flip(Flip)),
  asserta(dead(Player)).
