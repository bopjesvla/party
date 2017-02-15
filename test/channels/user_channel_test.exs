defmodule Mafia.UserChannelTest do
  use Mafia.ChannelCase

  alias Mafia.UserChannel

  setup do
    {:ok, _, socket} =
      socket = socket("user:0", %{user: 0})
      |> subscribe_and_join(UserChannel, "user:0")

    {:ok, socket: socket}
  end

  test "broadcasts are pushed to the client", %{socket: socket} do
    broadcast_from! socket, "broadcast", %{"some" => "data"}
    assert_push "broadcast", %{"some" => "data"}
  end
end
