:- use_module(library(pengines)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).

:- use_module(mafia).
:- use_module(registry).

:- pengine_application(afdas).

pengine_event(X) :- pengine_event(X, []).
pengine_ask(X, Y) :- pengine_ask(X, Y, []).

success :- pengine_event(E), E = success(_,_,_,_).
success(Error) :- success,!.
success(Error) :- reply_json_dict(m{error: Error}), fail.

handle(X) :-
    writeln(X),fail.

%handle(success(ID,_)) :-
    %pengine_property(ID, alias(reg)),
    %pengine_ask(ID, register(5), []),
    %pengine_ask(ID, register(5), []),
    %writeln(wow),
    %pengine_destroy(ID).

%:- pengine_create([id(Reg), destroy(false)]), pengine_event(create(ID, _)), pengine_ask(ID, register(5)), pengine_event(X).

create(Name, User, Setup, Result) :-
    pengine_ask(reg, register(Name)),
    success(name_taken),
    pengine_create([alias(Name)]),
    success(create_game_error),
    pengine_ask(Name, join(User), []),
    success(join_error).

read_request(Request, Values) :-
    http_read_json_dict(Request, Dict),
    Values :< Dict, !.

read_request(_, _) :-
    reply_json_dict(m{error: invalid_request}), fail.

handle_create(Request) :-
    read_request(Request, m{id: Id, user: User, setup: Setup}),
    create(Id, User, Setup, Result),
    reply_json_dict(Result),!.

handle_create(Request).

:- begin_tests(create).

test(q, [X = m{success: true}]) :- http_post('http://localhost:5000/create', json(m{id: asdf, user: 3, setup:[]}), X, []).

:- end_tests(create).

%create(Name, User, Setup) :-
    %pengine_ask(reg, register(Name), []),
    %pengine_create([alias(Name)]),
    %pengine_ask(Name, join(User), []).
