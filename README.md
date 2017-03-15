# [rty.party](https://rty.party)

The game of Mafia, also known as Werewolf, models a conspiracy of a
minority of players, who are aware of each other's identities, against an
uninformed majority.

rty is a real-time Mafia app, where games are played in something akin to a chat
room. The implementation is very flexible, supporting active roles, passive roles,
player replacements, multiple conspiring factions, role modifiers, and multiple
arbitrary roles per player, alignment, or entire village.

## Contributing

Most issues are starter issues. To contribute:

  * Install Nodejs, Elixir and PostgreSQL
  * Install dependencies with `mix deps.get`
  * Run `mix test` to verify everything is working
  * Make changes and add tests
  * Run `mix test` to verify everything is still working
  * Send a pull request

If you just want to alter the core game mechanics written in Prolog
(e.g. by adding a role or modifier),
you only need Elixir, and you can run the faster `mix test prolog` instead.

If you need help or advice, log onto [rty.party](https://rty.party) and locate
the dev room at the bottom of the sidebar.

## Running your own server

  * Install Nodejs, Elixir and PostgreSQL
  * Install dependencies with `mix deps.get`
  * Create and migrate your database with `mix ecto.create && mix ecto.migrate`
  * Install Node.js dependencies with `npm install`
  * Start Phoenix endpoint with `mix phoenix.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.
