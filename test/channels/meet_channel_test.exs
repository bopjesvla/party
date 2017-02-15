defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel, MeetChannel}

  @game %Mafia.Game{
    setup_id: 0,
    players: [
      %Mafia.GameSlot{status: "playing", user_id: 0},
      %Mafia.GameSlot{status: "playing", user_id: -1},
      %Mafia.GameSlot{status: "playing", user_id: -2},
      %Mafia.GameSlot{status: "playing", user_id: -3}
    ],
    channels: [
      %Mafia.Channel{user_id: 0, type: "game"},
      %Mafia.Channel{user_id: 0, type: "talk"}
    ]
  }

  test "can join talk, send messages" do
    Repo.insert!(%{@game | status: "signups", id: -3})
    |> Mafia.GameSupervisor.start_game

    socket = socket("user_socket:0", %{user: 0})
    |> subscribe_and_join!(MeetChannel, "talk:-3")

    push socket, "new:msg", %{"msg" => "there"}
    assert_broadcast "new:msg", %{msg: "there", u: 0, ts: _}
  end

  test "can vote" do
    Repo.insert!(%{@game | status: "ongoing", id: -4})
    |> Mafia.GameSupervisor.start_game

    {:ok, reply, socket} = socket("user_socket:0", %{user: 0})
    |> subscribe_and_join(GameChannel, "game:-4")

    assert %{active: [%{channel: global_channel, type: :global_role} | _]} = reply

    socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)

    push socket, "new:vote", %{"action" => "lynch", "targets" => [-1]}
    assert_broadcast "new:msg", %{u: _, type: "vote"}
  end

end
