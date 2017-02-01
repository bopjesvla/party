defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel,MeetChannel}

  @setup %{
    teams: [%{player: 1, team: "mafia"}, %{player: 2, team: "town"}, %{player: 3, team: "town"}, %{player: 4, team: "town"}],
    player_roles: [],
    alignment_roles: [],
    global_roles: [%{mods: [], role: "village"}],
    phases: ["day", "night"]
  }

  # setup do
  #   socket =
  #     socket("user:0", %{user: 0})
  #     |> subscribe_and_join!(GameChannel, "game:t", %{"setup" => @setup, "speed" => 10})

  #   ref = push socket, "info", %{}
  #   assert_reply ref, :ok, %{"active" => [%{"channel" => signups_channel}]}
   
  #   socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> signups_channel)
    
  #   {:ok, socket: socket}
  # end

  # test "ping replies with status ok", %{socket: socket} do
  #   ref = push socket, "ping", %{"hello" => "there"}
  #   assert_reply ref, :ok, %{"hello" => "there"}
  # end

  # test "broadcasts are pushed to the client", %{socket: socket} do
  #   broadcast_from! socket, "broadcast", %{"some" => "data"}
  #   assert_push "broadcast", %{"some" => "data"}
  # end

  # test "can send messages", %{socket: socket} do
  #   push socket, "new:msg", %{"msg" => "there"}
  #   assert_broadcast "new:msg", %{msg: "there", u: "bob", ts: _}
  # end
end
