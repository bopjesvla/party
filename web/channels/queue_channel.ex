defmodule Mafia.QueueChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Message, User, Game, GamePlayer, GameSupervisor, GameServer, Queries}
  import Ecto.Query

  def join("queue", payload, socket) do
    {:ok, socket}
  end

  def handle_in("new:setup", %{"setup" => setup, "speed" => speed} = opts, socket) do
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
      name: game.name,
      count: 1
    }

    {:reply, :ok, socket}
  end

  def handle_in("signup", %{"name" => name}, %{assigns: %{user: user}} = socket) do
    game = Repo.get_by(Game, name: name) |> Repo.preload(:setup)
    changeset = GamePlayer.changeset(%GamePlayer{user_id: user, game: game}, %{"status" => "playing"})

    {:ok, {res, count}} = Repo.transaction fn ->
      Repo.run! "lock table game_players in exclusive mode"

      count = Repo.one from p in GamePlayer,
      where: p.game_id == ^game.id,
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
          name: name,
          count: new_count
        }
        if new_count == game.setup.size do
          :erlang.send_after 10000, GameSupervisor, %{game: game}
        end
        :ok
      {:error, changeset} ->
        {:error, Mafia.ChangesetView.render("error.json", %{changeset: changeset})}
    end

    {:reply, reply, socket}
  end
end
