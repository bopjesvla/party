speed(10).
setup_team(1,"town").
setup_team(2,"town").
setup_team(3,"mafia").
setup_role(global, nil, ([], village)).
setup_role(team, "mafia", ([], killer)).
setup_role(player, 1, ([], cop)).
setup_phases([day, night]).

player(1).
player(3).
player(5).
