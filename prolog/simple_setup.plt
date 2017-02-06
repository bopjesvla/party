speed(10).
setup_alignment(1,"town").
setup_alignment(2,"town").
setup_alignment(3,"mafia").
setup_role(global, nil, ([], village)).
setup_role(alignment, "mafia", ([], killer)).
setup_role(player, 1, ([], cop)).
setup_phases([day, night]).
