defmodule Mafia.Queries do
  alias Mafia.{Repo, Game, Setup, GameServer}
  import Ecto.Query

  def setup_info(by) do
    setup = Repo.get_by!(Setup, by) |> Repo.preload(:user)

    roles = Repo.all from r in Mafia.SetupRole,
  	select: map(r, [:role, :mods, :type, :nr, :str]),
    where: r.setup_id == ^setup.id,
    order_by: r.nr

    teams = Repo.all from t in Mafia.SetupTeam,
  	select: map(t, [:player, :team]),
    where: t.setup_id == ^setup.id,
    order_by: t.player

  	setup
  	|> Map.take(~w(size phases name user id)a)
  	|> Map.merge(%{roles: roles, teams: teams, user: setup.user.name})
  end

  def game_info_and_messages(id, user) do
    info = game_info(id, user)
    
    query = if info.status == "ongoing" do
      channels = Enum.map(info.active ++ info.inactive, & &1.channel)
      from m in Mafia.Message,
        join: c in assoc(m, :channel),
        where: c.name in ^channels or (c.game_id == ^id and c.type != "meet")
    else
        from m in Mafia.Message,
          join: c in assoc(m, :channel),
          where: c.game_id == ^id
    end
    
    messages = query
    |> select([m], %{msg: m.msg, u: m.user_id, ts: m.inserted_at, ty: m.type})
    |> order_by(:inserted_at)
    |> Repo.all

    Map.put(info, :msgs, messages)
  end

  def game_info(id, user) do
    game = Repo.get_by(Game, id: id)
    |> Repo.preload(:setup)

  	players = Repo.all from g in Game,
  	join: p in assoc(g, :players),
  	join: u in assoc(p, :user),
  	where: g.id == ^game.id,
  	select: %{id: p.id, user: u.id, slot: p.game_slot_id, name: u.name, status: p.status}

    game_info = game
  	|> Map.take(~w(id speed status setup_id)a)
  	|> Map.merge(%{players: players, setup_name: game.setup.name})

    if game.status == "ongoing" do
      %{slot: slot_id} = Enum.find players, &(&1.user == user and &1.status == "playing")
      try do
        case GameServer.query(id, {:game_info, slot_id, {:info}}) do
          {:succeed, info: info} ->
            info
            |> Map.update!(:teams, &Enum.map(&1, fn x -> to_string(x) end))
            |> Map.merge(game_info)
          _ ->
            game_info
        end
      catch
        :exit, reason ->
          {1, _} = Game
          |> where(id: ^game.id, status: "ongoing")
          |> Repo.update_all(set: [status: "crashed"])

          Map.put(game_info, :status, "crashed")
          # exit(reason)
      end
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

  def player_for_slot(slot_id) do
    Repo.one! from p in Mafia.GamePlayer,
    where: p.game_slot_id == ^slot_id,
    order_by: [desc: p.inserted_at],
    limit: 1
  end
  
  def empty_slots do
    from s in Mafia.GameSlot,
    left_join: p in assoc(s, :game_players),
      on: p.game_slot_id == s.id and p.status == "playing",
    where: is_nil p.id
  end
end
