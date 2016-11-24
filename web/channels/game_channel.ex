defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Subchannel, Message, User, Supervisor, GameServer, Game}
  import Ecto.Query, only: [from: 2]

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def join("game:" <> name, opts = %{"setup" => _}, socket) do
    {:ok, pid} = Supervisor.start_game({name, socket.assigns.user, opts})
    IO.inspect 4356436543
    IO.inspect pid
    IO.inspect(:gproc.where {:n, :l, {:game, name}})
    {:ok, socket}
  end

  def join("game:" <> name, params, socket) do
    GameServer.join(name, socket.assigns.user)
    %{type: "g", id: id} = Repo.get_by!(Channel, name: name, type: "g") 

    messages = Repo.all from m in Message,
    join: u in assoc(m, :user),
    where: m.channel_id == ^id,
    select: %{msg: m.msg, u: u.name, ts: m.inserted_at}

    #channels = Repo.get_by(Channel, room_id: id)
    {:ok, %{msgs: messages}, socket}
  end

  def handle_in("new:msg", %{"msg" => msg}, socket) do
    "game:" <> name = socket.topic
    if String.printable?(msg) do
      %{id: id} = Repo.get_by Channel, name: name, type: "g"

      username = Repo.one! from u in User, where: u.id == ^socket.assigns.user, select: u.name
      %{inserted_at: inserted_at} = Repo.insert! Message.changeset(%Message{channel_id: id, user_id: socket.assigns.user}, %{type: "m", msg: msg})
      broadcast! socket, "new:msg", %{msg: msg, u: username, ts: inserted_at}
      {:noreply, socket}
    else
      {:error, "invalid message"}
    end
  end
end
