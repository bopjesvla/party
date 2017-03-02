defmodule Mafia.QueueChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Game, GameSlot, GamePlayer, GameSupervisor, Setup}
  import Ecto.Query

  @signups_countdown Application.get_env(:mafia, :signups_countdown)

  def join("queue", _payload, socket) do
    {:ok, socket}
  end

  def handle_in("list:games", _, socket) do
    games = Repo.all from g in Game,
    join: p in assoc(g, :players),
    join: s in assoc(g, :setup),
    where: p.status != "out" and g.status == "signups",
    group_by: [g.id, s.name, s.size],
    order_by: [desc: g.inserted_at],
    select: %{id: g.id, setup: s.name, size: s.size, count: count(p.id)},
    limit: 10

    {:reply, {:ok, %{games: games}}, socket}
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
      game: game.id,
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
    "signups" = game.status

    player = %GamePlayer{user_id: user, status: "playing"}

    nil = Mafia.Queries.player(id, user)

    res = Repo.run! :signup, [id, user, Ecto.DateTime.utc]

    reply = case res do
      %{num_rows: 1} ->
        new_count = Repo.one from p in GamePlayer,
        join: s in assoc(p, :game_slot),
        where: s.game_id == ^id and p.status == "playing",
        select: count(1)

        broadcast! socket, "count", %{
          id: id,
          count: new_count
        }
        Mafia.MeetChannel.new_message("talk:#{id}", "join", user, nil)
        Mafia.Endpoint.broadcast! "user:#{user}", "new:game", %{
          game: id,
          setup: game.setup.name,
          status: game.status,
          speed: game.speed
        }
        if new_count == game.setup.size do
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

              {:ok, _} = GameSupervisor.start_game(game)
            end
          end
        end
        :ok
      %{num_rows: 0} ->
        {:error, %{errors: %{"game": ["Already filled"]}}}
    end

    {:reply, reply, socket}
  end

  def handle_in("out", %{"id" => id, "status" => game_status}, socket) do
    # make sure the game didn't start while leaving
    %{status: ^game_status} = Repo.get!(Game, id)
    case game_status do
      "signups" ->
        Repo.all from p in GamePlayer,
        join: s in assoc(p, :game_slot),
        update: [set: [status: "out"]],
        where: p.user_id == ^socket.assigns.user and s.game_id == ^id
    end
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
