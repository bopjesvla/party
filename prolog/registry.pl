:- module(registry, [register/1]).

:- dynamic registered/1.

register(reg) :- !,fail.
register(Name) :- \+ registered(Name), assert(registered(Name)),!.
