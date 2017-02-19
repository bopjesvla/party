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

  def game_info(id, user) do
    game = Repo.get_by(Game, id: id)

  	setup = setup_info(game.setup_id)

  	players = Repo.all from g in Game,
  	join: p in assoc(g, :players),
  	join: u in assoc(p, :user),
  	where: g.id == ^game.id,
  	select: %{id: p.id, user: u.id, slot: p.game_slot_id, name: u.name}

    game_info = game
  	|> Map.take(~w(id speed status)a)
  	|> Map.merge(%{players: players, setup: setup})

    if game.status == "ongoing" do
      %{id: player_id} = Enum.find players, &(&1.user == user)
      # GameServer.query(id, {:player, {:player}}) |> raise
      {:succeed, info: info} = GameServer.query(id, {:game_info, player_id, {:info}})

      info
      |> to_map
      |> Map.merge(game_info)
    else
      game_info
    end
  end

  def player(game_id, user) do
    Repo.one from p in Mafia.GamePlayer,
    join: s in assoc(p, :game_slot),
    where: s.game_id == ^game_id and p.user_id == ^user and p.status != "out"
  end

  def player!(game_id, user) do
    Repo.one! from p in Mafia.GamePlayer,
    join: s in assoc(p, :game_slot),
    where: s.game_id == ^game_id and p.user_id == ^user and p.status != "out"
  end
end
