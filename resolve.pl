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

:- begin_tests(resolve).

test(resolve) :-
		set(current_phase(123)),
		resolve([
			       action(-101, kill, [-105], q),
			       action(-105, block, [-101], w),
			       action(-103, block, [-105], e),

			       action(-201, kill, [-202], protected),
			       action(-203, protect, [-202], t),
			       
			       action(-301, kill, [-302], y),
			       action(-303, protect, [-302], u),
			       action(-304, block, [-301], i),

			       action(-401, kill, [-402], protected),
			       action(-403, protect, [-402], u),
			       action(-406, protect, [-402], a),
			       action(-404, kill, [-402], protected),

			       % cross-kill cross-block paradox
			       action(-501, kill, [-502], paradox),
			       action(-502, kill, [-501], paradox),
			       action(-501, block, [-502], z),
			       action(-502, block, [-501], z)
			   ], _SuccessfulActions),
		unset(current_phase(123)).

test(blocked, all(X = [blocked])) :-
    action_history(123, action(-105, block, [-101], w), X).

test(not_blocked, [X = success]) :-
    action_history(123, action(-101, kill, [-105], q), X).

test(prefer_block_to_fail, all(X = [blocked])) :-
    action_history(123, action(-301, kill, _, y), X).

test(protects, all(X = [failed, failed, failed])) :-
    action_history(123, action(_, _, _, protected), X).

test(paradox, all(X = [success, success])) :-
    action_history(123, action(_, kill, _, paradox), X).

:- end_tests(resolve).
