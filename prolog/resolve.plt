:- begin_tests(resolve).

test(resolve) :-
    asserta(current_phase(123)),
    resolve([
           action(-101, kill, [-105], q, []),
           action(-105, block, [-101], w, []),
           action(-103, block, [-105], e, []),

           action(-201, kill, [-202], protected, []),
           action(-203, protect, [-202], t, []),
           
           action(-301, kill, [-302], y, []),
           action(-303, protect, [-302], u, []),
           action(-304, block, [-301], i, []),

           action(-401, kill, [-402], protected, []),
           action(-403, protect, [-402], u, []),
           action(-406, protect, [-402], a, []),
           action(-404, kill, [-402], protected, []),

           % cross-kill cross-block paradox
           action(-501, kill, [-502], paradox, []),
           action(-502, kill, [-501], paradox, []),
           action(-501, block, [-502], z, []),
           action(-502, block, [-501], z, [])
       ], _SuccessfulActions),
	retract(current_phase(123)).

test(blocked) :-
    action_history(123, action(-105, block, [-101], w, []), X),
    X = blocked.

test(not_blocked) :-
    action_history(123, action(-101, kill, [-105], q, []), X),
    X = success.

test(prefer_block_to_fail) :-
    action_history(123, action(-301, kill, _, y, []), X),
    X = blocked.

test(protects) :-
    findall(X, action_history(123, action(_, _, _, protected, []), X), Y),
    Y = [failed, failed, failed].

test(paradox) :-
    findall(X, action_history(123, action(_, kill, _, paradox, []), X), Y),
    Y = [success, success].

:- end_tests(resolve).
