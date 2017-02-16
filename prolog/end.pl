won_with_team(P) :-
  player_team(P, Team),
  team_won(Team).

end_game :-
  findall(X, ),
  findall(P, won(P), Won)

team_won("town") :-
  NotTown \= "town",
  alive(LivingPlayer),
  \+ player_team(LivingPlayer, NotTown), % there are no antitown folk alive
  player_team(LivingPlayer, "town"). % there is at least one townie alive

team_won(X) :-
  alive(Alive),
  count(Alive, AliveCount),
  count(player_team(Alive, X), BaddieCount),
  BaddieCount >= AliveCount / 2.
