won(P) :- player_won(P), !.
won(P) :- won_with_team(P).

won_with_team(P) :-
  player_team(P, Team),
  team_won(Team).

soft_end_game :-
  won_with_team(P),
  end_game.

end_game :-
  findall(P, won(P), Won),
  send(end_game(Won)).

team_won("town") :- !,
  NotTown \= "town",
  alive(LivingPlayer),
  \+ player_team(LivingPlayer, NotTown), % there are no antitown folk alive
  player_team(LivingPlayer, "town"). % there is at least one townie alive

team_won(Bad) :-
  count(alive(Alive), AliveCount),
  count((alive(Alive), player_team(Alive, Bad)), BaddieCount),
  BaddieCount >= AliveCount / 2.
