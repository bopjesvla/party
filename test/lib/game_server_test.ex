defmodule Mafia.GameServerTest do
  use ExUnit.Case
  alias Mafia.{GameServer,GameSupervisor}

  test "games can be restarted2" do
    assert {:ok, _} = GameSupervisor.start_child("y")
    assert :ok = GameServer.stop("y")
    assert {:ok, _} = GameSupervisor.start_child("y")
  end
end
