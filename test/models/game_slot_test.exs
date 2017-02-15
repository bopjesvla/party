defmodule Mafia.GameSlotTest do
  use Mafia.ModelCase

  alias Mafia.GameSlot

  @valid_attrs %{status: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = GameSlot.changeset(%GameSlot{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = GameSlot.changeset(%GameSlot{}, @invalid_attrs)
    refute changeset.valid?
  end
end
