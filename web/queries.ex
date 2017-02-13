defmodule Mafia.Queries do
  alias Mafia.{Repo, Message, User, Game, Setup, GameServer}
  import Ecto.Query

  def setup_info(id) do
    setup = Repo.get!(Setup, id) |> Repo.preload(:user)

    roles = Repo.all from s in Mafia.Setup,
	join: r in assoc(s, :roles),
	select: map(r, [:role, :mods, :nr, :str])

    teams = Repo.all from s in Mafia.Setup,
	join: r in assoc(s, :teams),
	select: map(r, [:player, :team])

	setup
	|> Map.take(~w(size phases name user)a)
	|> Map.merge(%{roles: roles, teams: teams, user: setup.user.name})
  end

  def to_map([{a, _} | _] = l) when is_atom(a) do
    for {a, x} <- l, into: %{} do
      {a, to_map x}
    end
  end

  def to_map(l) when is_list(l) do
    Enum.map l, &to_map/1
  end

  def to_map(x), do: x

  def game_info(name, user) do
    game = Repo.get_by(Game, name: name)

  	setup = setup_info(game.setup_id)

  	players = Repo.all from g in Game,
  	join: p in assoc(g, :players),
  	join: u in assoc(p, :user),
  	where: g.id == ^game.id,
  	select: %{id: p.id, user: u.id, name: u.name}

      game_info = game
  	|> Map.take(~w(name speed status)a)
  	|> Map.merge(%{players: players, setup: setup})

    if game.status == "ongoing" do
      {:succeed, info: game_info} = GameServer.query(name, {:game_info, user, {:info}})

      info = game_info
      |> to_map
      |> Map.merge(game)
    else
      game_info
    end
  end
end
