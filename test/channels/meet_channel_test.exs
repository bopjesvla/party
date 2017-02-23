defmodule Mafia.MeetChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel, MeetChannel}

  @game %Mafia.Game{
    setup_id: 0,
    slots: [
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: 0}]},
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -1}]},
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -2}]},
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -3}]}
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

  test "can vote, lynch" do
    game = Repo.insert!(%{@game | status: "ongoing", id: -4})
    {:ok, _} = game |> Mafia.GameSupervisor.start_game

    [first_slot | _] = game.slots

    {:ok, reply, socket} = socket("user_socket:0", %{user: 0})
    |> subscribe_and_join(GameChannel, "game:-4")

    assert %{active: [%{channel: global_channel, type: :global_role} | _]} = reply

    socket = socket |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)

    push socket, "new:vote", %{"action" => "lynch", "targets" => ["noone"]}
    assert_broadcast "new:msg", %{u: _, type: "vote"}
    push socket, "new:vote", %{"action" => "lynch", "targets" => [first_slot.id]}
    assert_broadcast "new:msg", %{u: _, type: "vote"}

    socket("user_socket:-1", %{user: -1})
    |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)
    |> push("new:vote", %{"action" => "lynch", "targets" => [first_slot.id]})

    socket("user_socket:-2", %{user: -2})
    |> subscribe_and_join!(MeetChannel, "meet:" <> global_channel)
    |> push("new:vote", %{"action" => "lynch", "targets" => [first_slot.id]})

    assert_broadcast "new:msg", %{msg: "has been lynched"}
  end

end
