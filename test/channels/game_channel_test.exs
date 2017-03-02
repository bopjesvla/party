defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.GameChannel

  @game %Mafia.Game{
    setup_id: 0,
    slots: [
      # mafia
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: 0}], setup_player: 1},
      # town
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -1}], setup_player: 2},
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -2}], setup_player: 3},
      %Mafia.GameSlot{game_players: [%Mafia.GamePlayer{status: "playing", user_id: -3}], setup_player: 4}
    ],
    channels: [
      %Mafia.Channel{user_id: 0, type: "game"},
      %Mafia.Channel{user_id: 0, type: "talk"}
    ]
  }

  test "can join game in signups, request game info" do
    Repo.insert! %{@game | status: "signups", id: -1}

    socket = socket("user_socket:0", %{user: 0})
    |> subscribe_and_join!(GameChannel, "game:-1")

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{id: -1}
  end

  test "can join ongoing game, request game info" do
    Repo.insert!(%{@game | status: "ongoing", id: -2})
    |> Mafia.GameSupervisor.start_game

    socket = socket("user_socket:0", %{user: 0})
    |> subscribe_and_join!(GameChannel, "game:-2")

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{id: -2, teams: ["mafia"], active: active}
    g = assert Enum.find(active, &(&1.type == :global_role))
  end
end
