defmodule Mafia.UserChannel do
  use Mafia.Web, :channel

  def join("user:" <> user, payload, socket) do
    if String.to_integer(user) == socket.assigns.user do
      games = Repo.all from g in Mafia.Game,
	  join: p in assoc(g, :players),
	  join: s in assoc(g, :setup),
	  join: u in assoc(p, :user),
	  where: u.id == ^socket.assigns.user and g.status == "ongoing",
	  select: %{
	    id: g.id,
		setup: s.name,
		speed: g.speed,
		status: g.status
      }
      {:ok, %{games: games}, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (user:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end
end
