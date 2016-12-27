defmodule Mafia.GameChannel do
  use Phoenix.Channel

  alias Mafia.{Repo, Channel, Message, User, Prolog, Game}
  import Ecto.Query, only: [from: 2]

  #def handle_in("room_info", %{name: name}, socket) do
    #case Repo.get_by(Room, name: name) do
      #nil -> nil
    #end
  #end

  
  def join("game:" <> name, opts = %{"setup" => setup}, socket = %{assigns: %{user: user}}) do
    prolog_setup = setup |> Prolog.prologize

    q = "mafia:set_setup(#{prolog_setup}), join(#{user}), game_info(#{user}, GameInfo)"

    changeset = Repo.insert!(%Channel{user_id: user, type: "g", name: name})

    {pengine, [%{"GameInfo" => game_info}]} = Prolog.create!(q)

    changeset
    |> Repo.preload(:game)
    |> Ecto.Changeset.change
    |> Ecto.Changeset.put_assoc(:game, %Game{pengine: pengine, status: "signups"})
    |> Repo.update!

    {:ok, game_info, socket}
  end

  def join("game:" <> name, params, socket = %{assigns: %{user: user}}) do
    %{type: "g", id: id, game: game} = Repo.get_by!(Channel, name: name, type: "g") |> Repo.preload(:game)

    Prolog.ask(game.pengine, "join(#{user}), game_info(#{user}, GameInfo)")

    messages = Repo.all from m in Message,
    join: u in assoc(m, :user),
    where: m.channel_id == ^id,
    select: %{msg: m.msg, u: u.name, ts: m.inserted_at}

    #channels = Repo.get_by(Channel, room_id: id)
    {:ok, %{msgs: messages}, socket}
  end
end
