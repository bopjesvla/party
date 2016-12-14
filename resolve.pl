resolve(Actions) :-
    forall(
	select(Action, Actions, Rest),
	resolve_action(Action, Rest)
    ),
    locked_actions(Actions),
    resolve_actions(Actions).

resolve_action(Action, Rest) :- stopped(Action, Rest), !.
resolve_action(action(Actor, Action, Targets, Channel), Rest) :-
    current_phase(P),
    assert(action_history(P, Actor, Action, Targets, Channel, endofphase)).

stopped(Action, Pool) :-
    select(Stopper, Pool, Remainder),
    stops(Stopper, Action, Remainder).

stops(Stopper, Action, Remainder) :- stops_unless_stopped(Stopper, Action), \+ pool_stops(Remainder, Stopped).

stops_unless_stopped(action(_, block, [Blocked], _), action(Blocked, _, _, _)).
