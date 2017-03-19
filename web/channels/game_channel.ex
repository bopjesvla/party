defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Game, GameSlot, GameSupervisor, GameServer, Queries}
  import Ecto.Query

  #def handle_in("room_info", %{id: id}, socket) do
    #case Repo.get_by(Room, id: id) do
      #nil -> nil
    #end
  #end

  def join("game:" <> id, _, %{assigns: %{user: user}} = socket) do
    id = String.to_integer(id)
    %{status: "playing"} = Queries.player!(id, user)

    # messages = Repo.run! :game_messages_for_user, [user, id]

    info = Queries.game_info(id, user)
    
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

    {:ok, Map.put(info, :msgs, messages), socket}
  end

  def new_message(game_id, type, user, message) do
    channel = Repo.get_by!(Channel, game_id: game_id, type: "game")
    %{inserted_at: inserted_at} = Repo.insert!(%Message{channel: channel, user_id: user, type: type, msg: message})

    Mafia.Endpoint.broadcast! "game:#{game_id}", "new:msg",
      %{msg: message, u: user, ts: inserted_at, ty: type}
  end

  def handle_in("info", _, socket) do
    "game:" <> id = socket.topic
    {:reply, {:ok, Queries.game_info(id, socket.assigns.user)}, socket}
  end
end
