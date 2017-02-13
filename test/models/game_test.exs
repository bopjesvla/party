defmodule Mafia.GameTest do
  use Mafia.ModelCase

  alias Mafia.Game

  @valid_attrs %{name: "asad-fdsf-4353", speed: 5}

  test "changeset with valid attributes" do
    changeset = Game.changeset(%Game{}, @valid_attrs)
    assert changeset.valid?
  end
end
