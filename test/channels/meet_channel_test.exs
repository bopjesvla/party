defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel,MeetChannel}

  @setup %{
    teams: [%{player: 1, team: "m"}, %{player: 2, team: "t"}, %{player: 3, team: "t"}, %{player: 4, team: "t"}],
    player_roles: [],
    alignment_roles: [],
    global_roles: [],
    phases: ["d", "n"]
  }

  setup do
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, "game:t", %{"setup" => @setup})
      |> subscribe_and_join!(MeetChannel, "meet:t:pre")

    {:ok, socket: socket}
  end

  test "ping replies with status ok", %{socket: socket} do
    ref = push socket, "ping", %{"hello" => "there"}
    assert_reply ref, :ok, %{"hello" => "there"}
  end

  test "broadcasts are pushed to the client", %{socket: socket} do
    broadcast_from! socket, "broadcast", %{"some" => "data"}
    assert_push "broadcast", %{"some" => "data"}
  end

  @tag :game
  test "can send messages", %{socket: socket} do
    ref = push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: "bob", ts: _}
  end
end
