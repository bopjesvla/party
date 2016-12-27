defmodule Mafia.GameChannelTest do
  use Mafia.ChannelCase

  alias Mafia.GameChannel

  @setup %{
    teams: [%{player: 1, team: "m"}, %{player: 2, team: "t"}, %{player: 3, team: "t"}, %{player: 4, team: "t"}],
    player_roles: [],
    alignment_roles: [],
    global_roles: [],
    setup_phases: ["d", "n"]
  }

  setup do
    socket =
      socket("user:0", %{user: 0})
      |> subscribe_and_join!(GameChannel, "game:x", %{"setup" => @setup})
      |> subscribe_and_join!(GameChannel, "game:x")

    {:ok, socket: socket}
  end
end
