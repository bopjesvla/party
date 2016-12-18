defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Subchannel, Message, User, Prolog, Game}
  import Ecto.Query, only: [from: 2]

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end
  
  def create_game(q) do
    body = Poison.encode! %{format: "json", destroy: false, ask: q}
    %{body: response} = HTTPoison.post! "localhost:5000/pengine/create", body, %{"Content-Type" => "application/json"}
    IO.warn response
    %{"id" => id, "event" => "create", "answer" => %{"data" => %{"event" => result}}} = x = Poison.decode! response
    IO.warn x
    ^result = "success"
    id
  end

  def ask_game(id, q) do
    %{body: response} = HTTPoison.get! "localhost:5000/pengine/send", [], params: %{format: "json", id: id, event: "ask(#{q}, [])"}
    Poison.decode! response
  end

  def join("game:" <> name, opts = %{"setup" => setup}, socket) do
    prolog_setup = setup |> Prolog.prologize

    q = "mafia:set_setup(#{prolog_setup}), join(#{socket.assigns.user})"

    %{id: channel} = Repo.insert! %Channel{user_id: socket.assigns.user, type: "g", name: name}
    Repo.insert! %Game{channel_id: channel, pengine: create_game(q)}

    {:ok, socket}
  end

  def join("game:" <> name, params, socket) do
    %{type: "g", id: id, game: game} = Repo.get_by!(Channel, name: name, type: "g") |> Repo.preload(:game)

    ask_game(game.pengine, "join(#{socket.assigns.user})")

    messages = Repo.all from m in Message,
    join: u in assoc(m, :user),
    where: m.channel_id == ^id,
    select: %{msg: m.msg, u: u.name, ts: m.inserted_at}

    #channels = Repo.get_by(Channel, room_id: id)
    {:ok, %{msgs: messages}, socket}
  end

  def handle_in("new:msg", %{"msg" => msg}, socket) do
    "game:" <> name = socket.topic
    if String.printable?(msg) do
      %{id: id} = Repo.get_by Channel, name: name, type: "g"

      username = Repo.one! from u in User, where: u.id == ^socket.assigns.user, select: u.name
      %{inserted_at: inserted_at} = Repo.insert! Message.changeset(%Message{channel_id: id, user_id: socket.assigns.user}, %{type: "m", msg: msg})
      broadcast! socket, "new:msg", %{msg: msg, u: username, ts: inserted_at}
      {:noreply, socket}
    else
      {:error, "invalid message"}
    end
  end
end
