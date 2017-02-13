defmodule Mafia.QueueChannelTest do
  use Mafia.ChannelCase

  alias Mafia.QueueChannel

  setup do
    {:ok, _, socket} =
      socket("user:0", %{user: 0})
      |> subscribe_and_join(QueueChannel, "queue")

    {:ok, socket: socket}
  end

  test "create game, signups", %{socket: socket} do
    name = "#{Enum.random(0..9000000)}"
    topic = "game:#{name}"

    ref = push socket, "new:game", %{"name" => "game1", "setup" => 0, "speed" => 10}
    assert_reply ref, :ok, _

    ref = push socket, "signup", %{"name" => "game1"}
    assert_reply ref, :ok, _

    {:ok, socket: socket, topic: topic, name: name}
  end

  # test "ping replies with status ok", %{socket: socket} do
  #   ref = push socket, "ping", %{"hello" => "there"}
  #   assert_reply ref, :ok, %{"hello" => "there"}
  # end
  #
  # test "shout broadcasts to queue:lobby", %{socket: socket} do
  #   push socket, "shout", %{"hello" => "all"}
  #   assert_broadcast "shout", %{"hello" => "all"}
  # end
  #
  # test "broadcasts are pushed to the client", %{socket: socket} do
  #   broadcast_from! socket, "broadcast", %{"some" => "data"}
  #   assert_push "broadcast", %{"some" => "data"}
  # end
end
