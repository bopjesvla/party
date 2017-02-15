defmodule Mafia.QueueChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Game, GamePlayer, GameSupervisor}
  import Ecto.Query

  @signups_countdown Application.get_env(:mafia, :signups_countdown)

  def join("queue", _payload, socket) do
    games = Repo.all from g in Game,
    join: p in assoc(g, :players),
    join: s in assoc(g, :setup),
    group_by: [g.id, s.name, s.size],
    select: %{id: g.id, setup: s.name, size: s.size, count: count(p.id)}

    {:ok, %{games: games}, socket}
  end

  def handle_in("new:setup", %{"setup" => setup}, socket) do
    {:ok, 1, socket}
  end

  def handle_in("new:game", %{"setup" => setup} = opts, %{assigns: %{user: user}} = socket) do
    player = GamePlayer.changeset(%GamePlayer{user_id: user}, %{"status" => "playing"})

    game = %Game{
      channels: [%Channel{user_id: user, type: "game"}, %Channel{user_id: user, type: "talk"}],
      status: "signups",
      setup_id: setup,
      players: [player]
    }
    |> Game.changeset(opts)
    |> Repo.insert!

    broadcast! socket, "game_info", %{
      id: game.id,
      count: 1
    }

    {:reply, {:ok, %{id: game.id}}, socket}
  end

  def handle_in("signup", %{"id" => id}, %{assigns: %{user: user}} = socket) do
    game = Repo.get!(Game, id) |> Repo.preload(:setup)
    changeset = GamePlayer.changeset(%GamePlayer{user_id: user, game: game}, %{"status" => "playing"})

    {:ok, {res, count}} = Repo.transaction fn ->
      Repo.run! "lock table game_players in exclusive mode"

      count = Repo.one from p in GamePlayer,
      where: p.game_id == ^id and p.status == "playing",
      select: count(1)

      res = if count < game.setup.size do
        Repo.insert changeset
      else
        {:error, Ecto.Changeset.add_error(changeset, :game, "Already filled")}
      end
      {res, count}
    end

    reply = case res do
      {:ok, _} ->
        new_count = count + 1
        broadcast! socket, "game_info", %{
          id: id,
          count: new_count
        }
        if new_count == game.setup.size do
          spawn fn ->
            Registry.register(:timer_registry, game.id, @signups_countdown)
            Process.sleep(@signups_countdown)

            {1, _} = Game
            |> where(id: ^game.id, status: "signups")
            |> Repo.update_all(set: [status: "ongoing"])

            Mafia.Endpoint.broadcast! "talk:#{id}", "leave", %{who: :all}

            {:ok, _} = GameSupervisor.start_game(game)
          end
        end
        :ok
      {:error, changeset} ->
        {:error, Mafia.ChangesetView.render("error.json", %{changeset: changeset})}
    end

    {:reply, reply, socket}
  end
end
