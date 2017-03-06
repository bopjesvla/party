defmodule Mafia.SetupRoleTest do
  use Mafia.ModelCase

  alias Mafia.SetupRole

  @valid_attrs %{mods: [], player: 42, role: "some content", type: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = SetupRole.changeset(%SetupRole{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = SetupRole.changeset(%SetupRole{}, @invalid_attrs)
    refute changeset.valid?
  end
end
