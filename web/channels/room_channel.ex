defmodule Mafia.RoomChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Room, Channel, Subchannel, Message}
  import Ecto.Query, only: [from: 2]

  def join("rooms", prams, socket) do
    {:ok, socket}
  end

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def join("room:" <> name, params, socket) do
    case Repo.get_by(Room, name: name) do
      nil ->
        channel = Channel.changeset(%Channel{})
                  |> Ecto.Changeset.put_assoc(:subchannels, [Channel.changeset(%Subchannel{})])

        changeset = Room.changeset(%Room{}, %{name: name, creator_id: socket.assigns.user, archived: false, type: "r"})
                    |> Ecto.Changeset.put_assoc(:channels, [channel])
                    |> Repo.insert!
        #Repo.insert! %Room{name: name, creator_id: socket.assigns.user, archived: false, type: "r"}
        #Repo.insert! %Channel{room_id: room_id}
        #Repo.insert! %Subchannel
        {:ok, socket}
      %{type: "r", id: id} ->
        messages = Repo.all from m in Message, join: sc in assoc(m, :subchannel), join: c in assoc(sc, :channel), where: c.room_id == ^id
        channels = Repo.get_by(Channel, room_id: id)
        {:ok, messages, socket}
    end
  end
end
