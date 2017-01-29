defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.{GameChannel}
  import Mock

  @setup %{
    teams: [%{player: 1, team: "m"}, %{player: 2, team: "t"}, %{player: 3, team: "t"}, %{player: 4, team: "t"}],
    player_roles: [],
    alignment_roles: [],
    global_roles: [%{mods: [], role: "village"}],
    phases: ["d", "n"]
  }

  setup do
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, "game:x", %{"setup" => @setup, "speed" => 10})
    {:ok, socket: socket}
  end


  test "can rejoin game", %{socket: socket} do
    socket("user:0", %{user: 0})
    |> subscribe_and_join!(GameChannel, "game:x")

    {:ok, socket: socket}
  end

  test "can request game info", %{socket: socket} do
    ref = push socket, "info", %{}
    assert_reply ref, :ok, %{"active" => [%{"channel" => _}]}
  end

  test "disconnects after signups", %{socket: socket} do
    with_mock Mafia.Pengine, [:passthrough], ask_after: fn(_, g, q) -> Mafia.Pengine.ask(g, q) end do
      socket("user:-1", %{user: -1})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      socket("user:-2", %{user: -2})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      ref = push socket, "info", %{}
      assert_reply ref, :ok, %{"active" => [%{"channel" => channel, "votes" => [], "actions" => [], "type" => "signups"}]}
      
      socket("user:-3", %{user: -3})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      :timer.sleep(1000)
      
      ref = push socket, "info", %{}
      assert_reply ref, :ok, %{"active" => [%{"channel" => _, "votes" => [], "actions" => [%{"act" => "lynch", "opt" => _}]}]}
      
      # socket("user:4", %{user: 4})
      # |> subscribe_and_join!(GameChannel, "game:x", %{})
    end
  end

  
  test "games are isolated", %{socket: socket} do
    with_mock Mafia.Pengine, [:passthrough], ask_after: fn(_, g, q) -> Mafia.Pengine.ask(g, q) end do
      socket("user:-1", %{user: -1})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      socket("user:-2", %{user: -2})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      ref = push socket, "info", %{}
      assert_reply ref, :ok, %{"active" => [%{"channel" => channel, "votes" => [], "actions" => [], "type" => "signups"}]}
      
      socket("user:-3", %{user: -3})
      |> subscribe_and_join!(GameChannel, "game:x", %{})
      
      socket =
        socket("user:-1", %{user: -1})
        |> subscribe_and_join!(GameChannel, "game:y", %{"setup" => @setup, "speed" => 10})

      socket =
        socket("user:-2", %{user: -3})
        |> subscribe_and_join!(GameChannel, "game:y", %{})

      socket =
        socket("user:-3", %{user: -3})
        |> subscribe_and_join!(GameChannel, "game:y", %{})      
    end
  end
end
