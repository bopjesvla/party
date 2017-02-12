defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Game, GameSupervisor, GameServer, Queries}
  import Ecto.Query

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  def render_message([msg, u, ts, type, ch]) do
    %{msg: msg, u: u, ts: ts, ty: type, ch: ch}
  end

  def join("game:" <> name, %{"setup" => setup, "speed" => speed} = opts, %{assigns: %{user: user}} = socket) when speed in 1..10000 do
    Repo.insert! %Game{name: name, channels: [%Channel{user_id: user, type: "g"}], setup_id: setup, speed: speed, status: "signups"}

    # {:ok, _} = GameSupervisor.start_game({name, user, setup, speed: speed})

    info = Queries.game_info(name, user)
    |> Map.put(:msgs, [])

    {:ok, info, socket}
  end

  def join("game:" <> name, params, %{assigns: %{user: user}} = socket) do
    %{rows: rows} = Ecto.Adapters.SQL.query!(Repo, "select * from messages_between_joins_and_kicks($1, $2)", [user, name])

    {:succeed, _} = GameServer.query(name, {:join, user})

    messages = Enum.map rows, &render_message/1

    info = Queries.game_info(name, user)
    |> Map.put(:msgs, messages)

    #channels = Repo.get_by(Channel, room_id: id)
    {:ok, info, socket}
  end

  def to_map([{a, _} | _] = l) when is_atom(a) do
    for {a, x} <- l, into: %{} do
      {a, to_map x}
    end
  end

  def external_message(game_name, type, user, message) do
    game = Repo.get_by(Channel, game: game_name, type: "g")
    channel = Repo.get_by(Channel, game: game, type: "g")
    %{inserted_at: inserted_at} = Repo.insert!(%Message{channel: channel, user_id: user, type: type, msg: message})

    Mafia.Endpoint.broadcast! "game:#{game_name}", "new:msg", %{msg: message, u: user, ts: inserted_at, type: type}
  end

  def handle_in("info", _, socket) do
    "game:" <> name = socket.topic
    {:reply, {:ok, Queries.game_info(name, socket.assigns.user)}, socket}
  end
end
