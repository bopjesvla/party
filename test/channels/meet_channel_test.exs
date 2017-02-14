defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel, MeetChannel, GameServer}

  @name "test"
  @topic "game:test"
  @game %Mafia.Game{
    name: @name,
    setup_id: 0,
    players: [
      %Mafia.GamePlayer{status: "playing", user_id: 0},
      %Mafia.GamePlayer{status: "playing", user_id: -1},
      %Mafia.GamePlayer{status: "playing", user_id: -2},
      %Mafia.GamePlayer{status: "playing", user_id: -3}
    ]
  }

  # test "ping replies with status ok", %{socket: socket} do
  #   ref = push socket, "ping", %{"hello" => "there"}
  #   assert_reply ref, :ok, %{"hello" => "there"}
  # end

  # test "broadcasts are pushed to the client", %{socket: socket} do
  #   broadcast_from! socket, "broadcast", %{"some" => "data"}
  #   assert_push "broadcast", %{"some" => "data"}
  # end

  # test "can send messages" do
  #   Repo.insert! %{@game | status: "signups"}
  #
  #   socket = socket("user:0", %{user: 0})
  #   |> subscribe_and_join!(MeetChannel, "talk:test")
  #
  #   push socket, "new:msg", %{"msg" => "there"}
  #   assert_broadcast "new:msg", %{msg: "there", u: 0, ts: _}
  # end
  #
  # test "can vote", %{socket: socket, topic: topic, name: name} do
  #   socket("user:-1", %{user: -1})
  #   |> subscribe_and_join!(GameChannel, topic, %{})
  #
  #   socket("user:-2", %{user: -2})
  #   |> subscribe_and_join!(GameChannel, topic, %{})
  #
  #   socket("user:-3", %{user: -3})
  #   |> subscribe_and_join!(GameChannel, topic, %{})
  #
  #   GameServer.query!(name, :remove_phase_timer)
  #   GameServer.query!(name, :next_phase)
  #
  #   ref = push socket, "info", %{}
  #   assert_reply ref, :ok, %{active: [%{channel: global_channel, type: :global_role} | _]}
  #
  #   socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)
  #
  #   push socket, "new:vote", %{"action" => "lynch", "targets" => [-1]}
  #   assert_broadcast "new:msg", %{u: _, type: "vote"}
  # end

end
