defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Pengine, Game, GameSupervisor, GameServer}
  import Ecto.Query

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def render_message([msg, u, ts, type, ch]) do
    %{msg: msg, u: u, ts: ts, ty: type, ch: ch}
  end
  
  def join("game:" <> name, %{"setup" => setup, "speed" => speed} = opts, %{assigns: %{user: user}} = socket) when speed in 1..10 do
    prolog_setup = %{setup: setup, speed: speed} |> Pengine.prologize

    Repo.insert! %Game{name: name, channels: [%Channel{user_id: user, type: "g"}]}
    
    {:ok, _} = GameSupervisor.start_game({name, user, setup})
    
    info = game_info(name, user)
    |> Map.put(:msgs, [])

    {:ok, info, socket}
  end

  def join("game:" <> name, params, %{assigns: %{user: user}} = socket) do
    %{rows: rows} = Ecto.Adapters.SQL.query!(Repo, "select * from messages_between_joins_and_kicks($1, $2)", [user, name])

    messages = Enum.map rows, &render_message/1

    info = game_info(name, user)
    |> Map.put(:msgs, messages)
    
    #channels = Repo.get_by(Channel, room_id: id)
    {:ok, info, socket}
  end

  def between_joins_and_kicks(events, target \\ :join)
  def between_joins_and_kicks([{"j", time} | events], :join) do
    "or inserted_at >= '#{time}' " <> between_joins_and_kicks(events, :kick)
  end
  def between_joins_and_kicks([{"k", time} | events], :kick) do
    "and inserted_at <= '#{time}' " <> between_joins_and_kicks(events, :join)
  end
  def between_joins_and_kicks([_ | events], target), do: between_joins_and_kicks(events, target)
  def between_joins_and_kicks([], _), do: ""

  def game_info(name, user) do
    {:succeed, info: game_info} = GameServer.prove(name, {:game_info, user, {:info}})

    info = game_info
    |> Enum.into(%{})
  end

  
  def handle_in("info", _, socket)    "game:" <> name = socket.topic
 do
    "game:" <> name = socket.topic
    {:reply, {:ok, game_info(name, socket.assigns.user)}, socket}
  end
end
