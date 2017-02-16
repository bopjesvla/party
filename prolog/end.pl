won_by_alignment(P) :-
  player_alignment(P, Alignment),
  alignment_won(Alignment).

end_game :-
  findall(X, ),
  findall(P, won(P), Won)

alignment_won("town") :-
  NotTown \= "town",
  alive(LivingPlayer),
  \+ player_alignment(LivingPlayer, NotTown), % there are no antitown folk alive
  player_alignment(LivingPlayer, "town"). % there is at least one townie alive

alignment_won(X) :-
  alive(Alive),
  count(Alive, AliveCount),
  count(player_alignment(Alive, X), BaddieCount),
  BaddieCount >= AliveCount / 2.
