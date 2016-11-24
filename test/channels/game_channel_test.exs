defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.GameChannel

  setup do
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, "game:x", %{"setup" => 1})
      |> subscribe_and_join!(GameChannel, "game:x")

    {:ok, socket: socket}
  end

  test "can send messages", %{socket: socket} do
    ref = push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: "bob", ts: _}
  end
end
