defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Game, GamePlayer, GameSupervisor, GameServer, Queries}
  import Ecto.Query

  #def handle_in("room_info", %{id: id}, socket) do
    #case Repo.get_by(Room, id: id) do
      #nil -> nil
    #end
  #end

  def render_message([msg, u, ts, type, ch]) do
    %{msg: msg, u: u, ts: ts, ty: type, ch: ch}
  end

  def join("game:" <> id, _, %{assigns: %{user: user}} = socket) do
    game = Repo.get!(Game, id)
    Repo.get_by!(GamePlayer, game_id: game.id, user_id: user, status: "playing")

    %{rows: rows} = Ecto.Adapters.SQL.query!(Repo, "select * from messages_between_joins_and_kicks($1, $2)", [user, id])

    messages = Enum.map rows, &render_message/1

    info = Queries.game_info(id, user)
    |> Map.put(:msgs, messages)
    {:ok, info, socket}
  end

  def new_message(game_id, type, user, message) do
    channel = Repo.get_by(Channel, game_id: game_id, type: "game")
    %{inserted_at: inserted_at} = Repo.insert!(%Message{channel: channel, user_id: user, type: type, msg: message})

    Mafia.Endpoint.broadcast! "game:#{game_id}", "new:msg",
      %{msg: message, u: user, ts: inserted_at, type: type}
  end

  def handle_in("info", _, socket) do
    "game:" <> id = socket.topic
    {:reply, {:ok, Queries.game_info(id, socket.assigns.user)}, socket}
  end
end
