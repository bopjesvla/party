defmodule Mafia.QueueChannelTest do
  use Mafia.ChannelCase

  alias Mafia.QueueChannel

  setup do
    {:ok, _, socket} =
      socket("user_socket:0", %{user: 0})
      |> subscribe_and_join(QueueChannel, "queue")

    {:ok, socket: socket}
  end

  test "create game, signups", %{socket: socket} do
    ref = push socket, "new:game", %{"setup_id" => 0, "speed" => 10}
    assert_reply ref, :ok, %{id: id}

    # Repo.get_by! Mafia.Message, %{type: "join", user_id: 0}

    assert_broadcast("new:game", %{id: ^id, size: 4})

    newsocket = socket("user_socket:-1", %{user: -1})
    |> subscribe_and_join!(QueueChannel, "queue")

    QueueChannel.broadcast_queue
    assert_broadcast "games", %{signups: [%{empty: 3, size: 4}]}

    ref = push(newsocket, "signup", %{"id" => id})
    assert_reply(ref, :ok, _)

    QueueChannel.broadcast_queue
    assert_broadcast "games", %{signups: [%{empty: 2, size: 4}]}

    ref = socket("user_socket:-2", %{user: -2})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :ok, _)

    ref = socket("user_socket:-3", %{user: -3})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :ok, _)

    ref = newsocket
    |> push("out", %{"id" => id})

    assert_reply(ref, :ok, _)

    ref = newsocket
    |> push("signup", %{"id" => id})

    assert_reply(ref, :ok, _)

    empty_mailbox()

    ref = socket("user_socket:-4", %{user: -4})
    |> subscribe_and_join!(QueueChannel, "queue")
    |> push("signup", %{"id" => id})

    assert_reply(ref, :error, %{errors: %{game: ["Already filled"]}})

    :timer.sleep(100)

    assert [%{game_id: ^id, setup_player: player}|_] = Repo.all Mafia.GameSlot
    assert is_integer(player)

    :ok = Mafia.GameServer.stop(id)
  end

  test "list setups", %{socket: socket} do
    ref = push socket, "list:setups", %{"search" => "Imp"}
    assert_reply ref, :ok, %{setups: ["Simple"]}
  end

  test "create setup", %{socket: socket} do
    setup = %{Mafia.Queries.setup_info(id: 0) | name: "OtherName"}
    ref = push socket, "new:setup", %{"setup" => setup}
    assert_reply ref, :ok, _
  end

  test "list roles", %{socket: socket} do
    ref = push socket, "role_info", %{}
    assert_reply ref, :ok, %{roles: roles, mods: mods}
    assert "weak" in mods
    assert "1-shot" in mods
    assert "doctor" in roles
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
