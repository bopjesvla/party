defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel, MeetChannel, GameServer}

  setup do
    name = "#{Enum.random(0..9000000)}"
    topic = "game:#{name}"

    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, topic, %{"setup" => 0, "speed" => 10})

    {:ok, socket: socket, topic: topic, name: name}
  end

  # test "ping replies with status ok", %{socket: socket} do
  #   ref = push socket, "ping", %{"hello" => "there"}
  #   assert_reply ref, :ok, %{"hello" => "there"}
  # end

  # test "broadcasts are pushed to the client", %{socket: socket} do
  #   broadcast_from! socket, "broadcast", %{"some" => "data"}
  #   assert_push "broadcast", %{"some" => "data"}
  # end

  test "can send messages", %{socket: socket} do
    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: signups_channel}]}

    socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> signups_channel)

    push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: 0, ts: _}
  end

  test "can vote", %{socket: socket, topic: topic, name: name} do
    socket("user:-1", %{user: -1})
    |> subscribe_and_join!(GameChannel, topic, %{})

    socket("user:-2", %{user: -2})
    |> subscribe_and_join!(GameChannel, topic, %{})

    socket("user:-3", %{user: -3})
    |> subscribe_and_join!(GameChannel, topic, %{})

    GameServer.query!(name, :remove_phase_timer)
    GameServer.query!(name, :next_phase)

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: global_channel, type: :global_role} | _]}

    socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)

    push socket, "new:vote", %{"action" => "lynch", "targets" => [-1]}
    assert_broadcast "new:msg", %{u: _, type: "vote"}
  end

end
