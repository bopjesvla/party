defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.GameChannel

  @name "test"
  @topic "game:test"
  @game %Mafia.Game{
    name: @name,
    setup_id: 0,
    players: [
      %Mafia.GamePlayer{status: "playing", user_id: 0},
      %Mafia.GamePlayer{status: "playing", user_id: -1},
      %Mafia.GamePlayer{status: "playing", user_id: -2},
      %Mafia.GamePlayer{status: "playing", user_id: -3}
    ]
  }

  test "can join game in signups, request game info" do
    Repo.insert! %{@game | status: "signups"}

    socket = socket("user:0", %{user: 0})
    |> subscribe_and_join!(GameChannel, @topic)

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{name: @name}
  end
end
