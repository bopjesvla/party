defmodule Mafia.QueueChannelTest do
  use Mafia.ChannelCase

  alias Mafia.QueueChannel

  setup do
    {:ok, _, socket} =
      socket("user:0", %{user: 0})
      |> subscribe_and_join(QueueChannel, "queue")

    {:ok, socket: socket}
  end

  defp empty_mailbox do
    receive do
      _ -> empty_mailbox()
    after
      0 -> :ok
    end
  end

  test "create game, signups", %{socket: socket} do
    ref = push socket, "new:game", %{"setup" => 0, "speed" => 10}
    assert_reply ref, :ok, %{id: id}

    assert_broadcast("game_info", %{id: ^id, count: 1})

    {:ok, reply, newsocket} = socket("user:-1", %{user: -1})
    |> subscribe_and_join(QueueChannel, "queue")

    assert %{games: [%{count: 1, size: 4}]} = reply

    ref = push(newsocket, "signup", %{"id" => id})
    assert_reply(ref, :ok, _)

    assert_broadcast("game_info", %{id: ^id, count: 2})

    ref = socket("user:-2", %{user: -2})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :ok, _)

    ref = socket("user:-3", %{user: -3})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :ok, _)

    empty_mailbox()

    ref = socket("user:-4", %{user: -4})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :error, %{errors: %{game: ["Already filled"]}})

    :timer.sleep(100)

    :ok = Mafia.GameServer.stop(id)
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
