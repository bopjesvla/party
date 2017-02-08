defmodule Mafia.RoomChannelTest do
  use Mafia.ChannelCase

  alias Mafia.RoomChannel

  setup do
    {:ok, _, socket} =
      socket("user:0", %{user: 0})
      |> subscribe_and_join(RoomChannel, "room:lobby")

    {:ok, socket: socket}
  end

  test "can send messages", %{socket: socket} do
    _ref = push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: "bob", ts: _}
  end
end
