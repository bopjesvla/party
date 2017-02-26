defmodule Mafia.UserChannel do
  use Mafia.Web, :channel

  def join("user:" <> user, payload, socket) do
    if String.to_integer(user) == socket.assigns.user do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end
  
  def handle_in("list:games", _, socket) do
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
    {:reply, %{games: games}, socket}
  end
end
