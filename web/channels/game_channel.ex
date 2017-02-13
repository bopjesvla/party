defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Game, GamePlayer, GameSupervisor, GameServer, Queries}
  import Ecto.Query

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def render_message([msg, u, ts, type, ch]) do
    %{msg: msg, u: u, ts: ts, ty: type, ch: ch}
  end

  def join("game:" <> name, _, %{assigns: %{user: user}} = socket) do
    game = Repo.get_by!(Game, name: name)
    Repo.get_by!(GamePlayer, game_id: game.id, user_id: user, status: "playing")

    %{rows: rows} = Ecto.Adapters.SQL.query!(Repo, "select * from messages_between_joins_and_kicks($1, $2)", [user, name])

    messages = Enum.map rows, &render_message/1

    info = Queries.game_info(name, user)
    |> Map.put(:msgs, messages)
    {:ok, info, socket}
  end

  def new_message(game_name, type, user, message) do
    game = Repo.get_by(Game, name: game_name)
    channel = Repo.get_by(Channel, game: game, type: "game")
    %{inserted_at: inserted_at} = Repo.insert!(%Message{channel: channel, user_id: user, type: type, msg: message})

    Mafia.Endpoint.broadcast! "game:#{game_name}", "new:msg", %{msg: message, u: user, ts: inserted_at, type: type}
  end

  def handle_in("info", _, socket) do
    "game:" <> name = socket.topic
    {:reply, {:ok, Queries.game_info(name, socket.assigns.user)}, socket}
  end
end
