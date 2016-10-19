defmodule Mafia.RoomChannel do
  alias Mafia.{Repo, Room, RoomMessage}
  import Ecto.Query, only: [from: 2]

  def join("room:" <> name, params, socket) do
    case Repo.get_by(Room, name: name) do
      nil ->
        #user = Repo.get(User, socket.assigns.user)
        #IO.inspect user
        #Repo.insert! %Room{name: name, creator: user, archived: false}
        {:ok, socket}
      %{type: "r", id: id} ->
        messages = Repo.all from m in Message, join: c in assoc(m, :channel), where: c.id == ^id
        {:ok, messages, socket}
    end
  end
end
