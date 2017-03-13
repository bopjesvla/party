# [rty.party](https://rty.party)

## Contributing

  * Install Nodejs, Elixir and PostgreSQL
  * Install dependencies with `mix deps.get`
  * Run `mix test` to verify everything is working
  * Make a change, add tests, and run `mix test` until you've convinced your computer your code is correct
  * Send a pull request

If you just want to alter core game mechanics (e.g. by adding a role or modifier),
you only need Elixir, and you can run `mix test prolog` instead.

## Running your own server

  * Install Nodejs, Elixir and PostgreSQL
  * Install dependencies with `mix deps.get`
  * Create and migrate your database with `mix ecto.create && mix ecto.migrate`
  * Install Node.js dependencies with `npm install`
  * Start Phoenix endpoint with `mix phoenix.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.
