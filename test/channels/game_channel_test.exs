defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel, GameServer}

  @setup %{
    teams: [%{player: 1, team: "mafia"}, %{player: 2, team: "town"}, %{player: 3, team: "town"}, %{player: 4, team: "town"}],
    roles: [%{type: "global", player: nil, team: nil, mods: [], role: "village"}],
    phases: ["day", "night"]
  }

  setup do
    name = "#{Enum.random(0..9000000)}"
    topic = "game:#{name}"
    
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, topic, %{"setup" => @setup, "speed" => 10})

    {:ok, socket: socket, topic: topic, name: name}
  end

  test "can rejoin game", %{topic: topic} do
    socket("user:0", %{user: 0})
    |> subscribe_and_join!(GameChannel, topic)
  end

  test "can request game info", %{socket: socket} do
    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: _}]}
  end

  test "signups", %{socket: socket, topic: topic, name: name} do
    socket("user:-1", %{user: -1})
    |> subscribe_and_join!(GameChannel, topic, %{})
    
    socket("user:-2", %{user: -2})
    |> subscribe_and_join!(GameChannel, topic, %{})
    
    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: channel, votes: [], actions: [], type: signups}], phase: %{name: nil, number: nil, next: nil}, players: _}
    
    socket("user:-3", %{user: -3})
    |> subscribe_and_join!(GameChannel, topic, %{})

    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{active: [%{channel: channel, votes: [], actions: [], type: signups}], phase: %{name: nil, number: nil, next: np}, players: players}
    
    assert is_number(np)
    
    GameServer.query!(name, :remove_phase_timer)
    GameServer.query!(name, :next_phase)
    
    ref = push socket, "info", %{}
    assert_reply ref, :ok, (%{active: [%{channel: _, votes: [], actions: [%{act: :lynch, opt: _} | _]}, _]} = info)
    
    assert %{phase: %{name: :day, number: 1, next: np2}} = info
    
  #   socket("user:4", %{user: 4})
  #   |> subscribe_and_join!(GameChannel, topic, %{})
  end

  
  # test "games are isolated", %{socket: socket} do
  #   socket("user:-1", %{user: -1})
  #   |> subscribe_and_join!(GameChannel, topic, %{})
    
  #   socket("user:-2", %{user: -2})
  #   |> subscribe_and_join!(GameChannel, "game:x", %{})
    
  #   ref = push socket, "info", %{}
  #   assert_reply ref, :ok, %{"active" => [%{"channel" => channel, "votes" => [], "actions" => [], "type" => "signups"}]}
    
  #   socket("user:-3", %{user: -3})
  #   |> subscribe_and_join!(GameChannel, "game:x", %{})
    
  #   socket =
  #     socket("user:-1", %{user: -1})
  #     |> subscribe_and_join!(GameChannel, "game:y", %{"setup" => @setup, "speed" => 10})

  #   socket =
  #     socket("user:-2", %{user: -3})
  #     |> subscribe_and_join!(GameChannel, "game:y", %{})

  #   socket =
  #     socket("user:-3", %{user: -3})
  #     |> subscribe_and_join!(GameChannel, "game:y", %{})      
  # end
end
