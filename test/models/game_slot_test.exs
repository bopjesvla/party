defmodule Mafia.GameSlotTest do
  use Mafia.ModelCase

  alias Mafia.GameSlot

  @valid_attrs %{}

  test "changeset with valid attributes" do
    changeset = GameSlot.changeset(%GameSlot{}, @valid_attrs)
    assert changeset.valid?
  end
end
