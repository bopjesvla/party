defmodule Mafia.MeetChannel do
  use Mafia.Web, :channel

  alias Mafia.{Repo, Channel, Message, User, Pengine, Game, GameServer}

  def meet_channel(%{topic: "meet:" <> name}) do
     Repo.get_by(Channel, name: name, type: "m") |> Repo.preload(:game)
  end

  def join("meet:" <> meet, payload, socket) do
    meet_channel = Repo.get_by!(Channel, name: meet, type: "m") |> Repo.preload(:game)

    GameServer.query! meet_channel.game.name, {:join_channel, socket.assigns.user, meet}

    {:ok, socket}
  end

  def handle_in("new:msg", %{"msg" => msg}, socket) do
    "meet:" <> meet = socket.topic
    if String.printable?(msg) do
      %{id: id} = Repo.get_by(Channel, name: meet, type: "m")

      %{inserted_at: inserted_at} = Repo.insert! Message.changeset(%Message{channel_id: id, user_id: socket.assigns.user}, %{type: "m", msg: msg})
      broadcast! socket, "new:msg", %{msg: msg, u: socket.assigns.user, ts: inserted_at, type: "m"}
      {:noreply, socket}
    else
      {:error, "invalid message"}
    end
  end

  def handle_in("new:vote", %{"action" => action, "targets" => targets}, socket) do
    %{game: game, name: name} = meet_channel(socket)
    GameServer.query! game.name, {:vote, socket.assigns.user, name, String.to_existing_atom(action), targets}
    {:reply, :ok, socket}
  end
  
  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end
  
  intercept ["leave"]
  def handle_out("leave", msg, socket) do
    push socket, "leave", msg
    
    %{who: who} = Poison.decode! msg
    case who do
      "all" ->
        {:stop, :normal, socket}
      who ->
        if socket.assigns.user in who do
          {:stop, :normal, socket}
        else
          {:noreply, socket}
        end
    end
  end
  
  def external_message(meet, type, user, message) do
    channel = Repo.get_by(Channel, name: meet, type: "m")
    %{inserted_at: inserted_at} = Repo.insert!(%Message{channel: channel, user_id: user, type: type, msg: message})
    
    Mafia.Endpoint.broadcast! "meet:#{meet}", "new:msg", %{msg: message, u: user, ts: inserted_at, type: type}
  end
  
  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
