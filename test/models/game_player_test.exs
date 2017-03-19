defmodule Mafia.GamePlayerTest do
  use Mafia.ModelCase

  alias Mafia.GamePlayer

  @valid_attrs %{status: "playing", game_slot_id: 1}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = GamePlayer.changeset(%GamePlayer{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = GamePlayer.changeset(%GamePlayer{}, @invalid_attrs)
    refute changeset.valid?
  end
end
