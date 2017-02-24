# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Mafia.Repo.insert!(%Mafia.SomeModel{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias Mafia.{Repo,User,Setup}

Repo.insert!(%User{id: 0, name: "bob", email: "a@b.nl", password_hash: "$2b$12$96hYKTymlpk9fR8bWsjYw.4VUbhVhiN1P5CZI6uf1ceQ.gQVbZNTi"})
Repo.insert!(%User{id: -1, name: "bob1", email: "a@b.nl1", password_hash: "$2b$12$96hYKTymlpk9fR8bWsjYw.4VUbhVhiN1P5CZI6uf1ceQ.gQVbZNTi"})
Repo.insert!(%User{id: -2, name: "bob2", email: "a@b.nl2", password_hash: "$2b$12$96hYKTymlpk9fR8bWsjYw.4VUbhVhiN1P5CZI6uf1ceQ.gQVbZNTi"})
Repo.insert!(%User{id: -3, name: "bob3", email: "a@b.nl3", password_hash: "$2b$12$96hYKTymlpk9fR8bWsjYw.4VUbhVhiN1P5CZI6uf1ceQ.gQVbZNTi"})
Repo.insert!(%User{id: -4, name: "bob4", email: "a@b.nl4", password_hash: "$2b$12$96hYKTymlpk9fR8bWsjYw.4VUbhVhiN1P5CZI6uf1ceQ.gQVbZNTi"})

Repo.insert!(%Setup{
  id: 0,
  user_id: 0,
  name: "Simple",
  size: 4,
  teams: [%{player: 1, team: "mafia"}, %{player: 2, team: "town"}, %{player: 3, team: "town"}, %{player: 4, team: "town"}],
  roles: [%{type: "global", nr: nil, str: nil, mods: [], role: "village"}, %{type: "team", nr: nil, str: "mafia", mods: [], role: "killer"}],
  phases: ["day", "night"]
})
