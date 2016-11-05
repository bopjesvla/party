defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Subchannel, Message, User, GameServer, Game}
  import Ecto.Query, only: [from: 2]

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def join("game:" <> game, opts = %{setup: setup}, socket) do
    channel = %Channel{user_id: socket.assigns.user, type: "g"}
    game = Repo.insert! %Game{name: game, user_id: socket.assigns.user, seed: 5, setup: setup}
    GameServer.start_link(game, socket.assigns.user, channel, opts)
  end

  def join("game:" <> name, params, socket) do
    case Repo.get_by(Channel, name: name, type: "r") do
      nil ->
        Repo.transaction(fn ->
          channel = %Channel{name: name, user_id: socket.assigns.user, type: "r", active_subchannel: %Subchannel{}}
                    |> Repo.insert!

          channel.active_subchannel
          |> Ecto.Changeset.change(channel_id: channel.id)
          |> Repo.update!
          #Repo.insert! %Room{name: name, creator_id: socket.assigns.user, archived: false, type: "r"}
          #Repo.insert! %Channel{room_id: room_id}
          #Repo.insert! %Subchannel
        end)
        {:ok, [], socket}
      %{type: "r", id: id} ->
        messages = Repo.all from m in Message,
        join: sc in assoc(m, :subchannel),
        join: u in assoc(m, :user),
        where: sc.channel_id == ^id,
        select: %{msg: m.msg, u: u.name, ts: m.inserted_at}

        #channels = Repo.get_by(Channel, room_id: id)
        {:ok, messages, socket}
    end
  end

  def handle_in("new_msg", %{"msg" => msg}, socket) do
    "room:" <> name = socket.topic
    if String.printable?(msg) do
      %{active_subchannel_id: active_id} = Repo.get_by Channel, name: name, type: "r"

      username = Repo.one! from u in User, where: u.id == ^socket.assigns.user, select: u.name
      %{inserted_at: inserted_at} = Repo.insert! %Message{type: "m", msg: msg, subchannel_id: active_id, user_id: socket.assigns.user}
      broadcast! socket, "new_msg", %{msg: msg, u: username, ts: inserted_at}
      {:noreply, socket}
    else
      {:error, "invalid message"}
    end
  end
end
