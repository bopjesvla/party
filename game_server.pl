:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(pengine_sandbox:mafia).

:- include(utils).

create_game(Res) :- http_post('http://localhost:5000/pengine/create', json(m{format: json, destroy: false}), Res, []).

:- http_server(http_dispatch, [port(5000)]).

:- begin_tests(game_server).

test(utils_scoping) :- set(a(x)), a(x).

test(create, [X = json([event=create,id=_,_])]) :- create_game(X).

test(ask_single, [Event = success]) :-
    create_game(json([event=create,id=Id,_])),
    http_get([host(localhost), port(5000), path('/pengine/send'), search([format='json',id=Id,event='ask(assert(setup_alignment(1,2)),[])'])], json(X), []),
    member(event=Event, X),!.

:- end_tests(game_server).
