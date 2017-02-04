defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel,MeetChannel}

  @setup %{
    teams: [%{player: 1, team: "mafia"}, %{player: 2, team: "town"}, %{player: 3, team: "town"}, %{player: 4, team: "town"}],
    roles: [%{type: "global", player: nil, team: nil, mods: [], role: "village"}],
    phases: ["day", "night"]
  }

  setup do
    topic = "game:#{Enum.random(0..9000000)}"
    
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, topic, %{"setup" => @setup, "speed" => 10})

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: signups_channel}]}

    socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> signups_channel)
    
    {:ok, socket: socket}
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
    push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: 0, ts: _}
  end
end
