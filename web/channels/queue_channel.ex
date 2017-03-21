defmodule Mafia.QueueChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Game, GameSlot, GamePlayer, GameSupervisor, Setup}
  import Ecto.Query

  @signups_countdown Application.get_env(:mafia, :signups_countdown)

  def join("queue", _payload, socket) do
    {:ok, socket}
  end

  def broadcast_queue do
    signups = Repo.all from e in Mafia.Queries.empty_slots,
    join: g in assoc(e, :game),
    join: s in assoc(g, :setup),
    group_by: [g.id, s.name, s.size],
    order_by: [desc: g.inserted_at],
    where: g.status == "signups",
    select: %{id: g.id, setup: s.name, size: s.size, empty: count(e.id)},
    limit: 10

    replacements = Repo.all from e in Mafia.Queries.empty_slots,
    join: g in assoc(e, :game),
    join: s in assoc(g, :setup),
    order_by: [desc: g.inserted_at],
    where: g.status == "ongoing",
    select: %{id: g.id, setup: s.name},
    limit: 10

    Mafia.Endpoint.broadcast! "queue", "games", %{signups: signups, replacements: replacements}
  end

  def handle_in("new:setup", %{"setup" => setup}, socket) do
    reply = %Setup{user_id: socket.assigns.user}
    |> Setup.changeset(setup)
    |> Repo.insert
    |> case do
      {:error, changeset} ->
        {:error, Mafia.ChangesetView.render("error.json", %{changeset: changeset})}
      {:ok, g} -> {:ok, %{id: g.id}}
    end
    {:reply, reply, socket}
  end

  def handle_in("new:game", %{"setup_id" => setup_id} = opts, %{assigns: %{user: user}} = socket) do
    setup = Repo.get!(Mafia.Setup, setup_id)
    [first_setup_player | rest] = Enum.shuffle(1..setup.size)

    player = %GamePlayer{user_id: user, status: "playing"}

    player_slot = %GameSlot{game_players: [player], setup_player: first_setup_player}
    other_slots = Enum.map(rest, &%GameSlot{setup_player: &1})

    game = %Game{
      channels: [%Channel{user_id: user, type: "game"}, %Channel{user_id: user, type: "talk"}],
      status: "signups",
      setup_id: setup_id,
      slots: [player_slot | other_slots]
    }
    |> Game.changeset(opts)
    |> Repo.insert!

    broadcast! socket, "new:game", %{
      id: game.id,
      count: 1,
      setup: setup.name,
      size: setup.size
    }

    Mafia.Endpoint.broadcast! "user:#{user}", "new:game", %{
      id: game.id,
      setup: setup.name,
      status: game.status,
      speed: game.speed
    }

    Mafia.MeetChannel.new_message("talk:#{game.id}", "join", user, nil)

    if Mix.env == :dev and user == 0 do
      Enum.each 2..setup.size, fn n ->
        handle_in("signup", %{"id" => game.id}, assign(socket, :user, -n))
      end
    end

    {:reply, {:ok, %{id: game.id}}, socket}
  end

  def handle_in("signup", %{"id" => id}, %{assigns: %{user: user}} = socket) do
    game = Repo.get!(Game, id) |> Repo.preload(:setup)

    unless game.status in ~w(signups ongoing) do
      raise "can't sign up to inactive games"
    end

    nil = Mafia.Queries.player(id, user)

    empty_slot_id = Repo.one from e in Mafia.Queries.empty_slots,
    where: e.game_id == ^id,
    limit: 1,
    select: e.id

    res = %GamePlayer{user_id: user}
    |> GamePlayer.changeset(%{
      game_slot_id: empty_slot_id,
      status: "playing",
    })
    |> Repo.insert

    reply = case res do
      {:ok, _} ->
        empty_slot = Repo.one from e in Mafia.Queries.empty_slots,
        where: e.game_id == ^id,
        limit: 1,
        select: true

        Mafia.MeetChannel.new_message("talk:#{id}", "join", user, nil)
        Mafia.Endpoint.broadcast! "user:#{user}", "new:game", %{
          id: id,
          setup: game.setup.name,
          status: game.status,
          speed: game.speed
        }

        if game.status == "signups" and empty_slot == nil do
          spawn fn ->
            Registry.register(:timer_registry, game.id, @signups_countdown)
            Process.sleep(@signups_countdown)

            [{pid, _}] = Registry.lookup(:timer_registry, game.id)
            if pid == self() do
              game = Repo.preload(game, :slots)

              {1, _} = Game
              |> where(id: ^game.id, status: "signups")
              |> Repo.update_all(set: [status: "ongoing"])

              Mafia.Endpoint.broadcast! "talk:#{id}", "leave", %{who: :all}
              broadcast! socket, "status", %{id: game.id, status: "ongoing"}

              {:ok, _} = GameSupervisor.start_game(game)
            end
          end
        end
        :ok
      {:error, _} ->
        {:error, %{errors: %{"game": ["Already filled"]}}}
    end

    {:reply, reply, socket}
  end

  def handle_in("out", %{"id" => id}, socket) do
    game = Repo.get!(Game, id)

    unless game.status in ~w(signups ongoing) do
      raise "can't leave inactive games"
    end

    new_player_status = if game.status == "ongoing", do: "replaced", else: "out"

    query = from p in GamePlayer,
    join: s in assoc(p, :game_slot),
    where: p.user_id == ^socket.assigns.user and s.game_id == ^id

    Repo.update_all(query, set: [status: new_player_status])

    Mafia.Endpoint.broadcast! "user:#{socket.assigns.user}", "leave:game", %{
      id: id
    }

    {:reply, :ok, socket}
  end

  def handle_in("list:setups", %{"search" => search}, socket) do
    names = Repo.all from s in Mafia.Setup,
    where: ilike(s.name, ^("%#{search}%")),
    select: s.name
    {:reply, {:ok, %{setups: names}}, socket}
  end

  def handle_in("role_info", _, socket) do
    {{:succeed, [info: info]}, _} = :erlog.prove({:role_info, {:info}}, Mafia.GameServer.game_db)
    roles = Enum.map info[:roles], &to_string/1
    mods = Enum.map info[:mods], &to_string/1
    {:reply, {:ok, %{roles: roles, mods: mods}}, socket}
  end

  def handle_in("setup_info", params, socket) do
    by = case params do
      %{"name" => name} ->
        [name: name]
      %{"game_id" => game_id} ->
        [id: Repo.get!(Game, game_id).setup_id]
    end
    info = Mafia.Queries.setup_info(by)
    {:reply, {:ok, %{setup: info}}, socket}
  end
end
