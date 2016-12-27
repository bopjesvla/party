defmodule Mafia.MeetChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Message, User, Prolog, Game}

  def join("meet:" <> meet, payload, socket) do
    [game_name, channel] = String.split meet, ":"
    %{game: game} = Repo.get_by!(Channel, name: game_name, type: "g") |> Repo.preload(:game)
    
    Prolog.ask! game.pengine, "join_channel(#{socket.assigns.user}, #{Prolog.prologize channel})"

    Repo.insert(%Channel{name: meet, game: game, type: "m"})

    {:ok, socket}
  end

  def handle_in("new:msg", %{"msg" => msg}, socket) do
    "meet:" <> meet = socket.topic
    if String.printable?(msg) do
      %{id: id} = Repo.get_by(Channel, name: meet, type: "m")

      username = Repo.one! from u in User, where: u.id == ^socket.assigns.user, select: u.name
      %{inserted_at: inserted_at} = Repo.insert! Message.changeset(%Message{channel_id: id, user_id: socket.assigns.user}, %{type: "m", msg: msg})
      broadcast! socket, "new:msg", %{msg: msg, u: username, ts: inserted_at}
      {:noreply, socket}
    else
      {:error, "invalid message"}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
