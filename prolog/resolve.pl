% based on http://wiki.mafiascum.net/index.php?title=Reasonable_Action_Resolution

resolve(Actions, SuccessfulActions) :-
    current_phase(P),
    findall(Action, (
		select(Action, Actions, Rest),
		resolve_action(Action, Rest, Result),
		asserta(action_history(P, Action, Result)),
		Result = success % after assertion so blocked actions are logged too
	    ), SuccessfulActions).

resolve_action(Action, Rest, blocked) :-
    stopped(Action, Rest, blocked), !.

% only make an action fail after it has been established it can't be stopped
% for example, if a kill target is both protected and rolestopped, the kill should be stopped, not fail
resolve_action(Action, Rest, failed) :- 
    stopped(Action, Rest, failed), !.

resolve_action(Action, _, success).

stopped(Action, Pool, Res) :-
    select(Stopper, Pool, Rest),
    stops(Stopper, Action, Rest, Res).

stops(Stopper, Action, Rest, blocked) :- blocks(Stopper, Action), \+ stopped(Stopper, Rest, _Anyhow).
stops(Stopper, Action, Rest, failed) :- makes_fail(Stopper, Action), \+ stopped(Stopper, Rest, _Anyhow).

blocks(action(_, block, [Player], _), action(Player, _, _, _)).
blocks(action(_, rolestop, [Target], _), action(_, _, Targets, _)) :- member(Target, Targets).

makes_fail(X, Y) :- protects(X, Target), kills(Y, Target).
makes_fail(X, Y) :- protects(Y, Target), kills(X, Target).

protects(action(_, protect, [Target], _), Target).
kills(action(_, kill, [Target], _), Target).
