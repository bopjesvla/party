defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Pengine, Game}
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

    q = "mafia:setup_game(#{prolog_setup}), create_channel(signups, none, _), join(<%= user %>), game_info(<%= user %>, GameInfo)"

    game_channel = Repo.insert!(%Channel{user_id: user, type: "g", name: name})

    {pengine, %{"GameInfo" => game_info}} = Pengine.create!(q, user: user)
    
    game_channel
    |> Repo.preload(:game)
    |> Ecto.Changeset.change
    |> Ecto.Changeset.put_assoc(:game, %Game{pengine: pengine, status: "signups"})
    |> Repo.update!

    info = Map.put(game_info, :msgs, [])

    {:ok, info, socket}
  end

  def join("game:" <> name, params, %{assigns: %{user: user}} = socket) do
    channel = Repo.get_by!(Channel, name: name, type: "g") |> Repo.preload(:game)

    %{"GameInfo" => info} = Pengine.ask!(channel.game, "join(<%= user %>), game_info(<%= user %>, GameInfo)", user: user)

    %{rows: rows} = Ecto.Adapters.SQL.query!(Repo, "select * from messages_between_joins_and_kicks($1, $2)", [user, channel.game_id])

    messages = Enum.map rows, &render_message/1

    info = Map.put(info, :msgs, messages)
    
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

  def game_info(game, socket) do
    x = Pengine.ask! game, "game_info(<%= user %>, GameInfo)", user: socket.assigns.user
    x["GameInfo"]
  end

  
  def handle_in("info", _, socket) do
    "game:" <> name = socket.topic
    channel = Repo.get_by!(Channel, name: name, type: "g") |> Repo.preload(:game)

    {:reply, {:ok, game_info(channel.game, socket)}, socket}
  end
end
