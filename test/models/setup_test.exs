defmodule Mafia.SetupTest do
  use Mafia.ModelCase

  alias Mafia.Setup

  @valid_attrs %{
    name: "Simple",
    teams: [%{player: 1, team: "mafia"}, %{player: 2, team: "town"}, %{player: 3, team: "town"}, %{player: 4, team: "town"}],
    roles: [%{type: "global", nr: nil, str: nil, mods: [], role: "village"}],
    phases: ["day", "night"],
    size: 4
  }
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Setup.changeset(%Setup{}, @valid_attrs)
    assert changeset.errors == []
    assert [%{valid?: true} | _] = changeset.changes.roles
  end

  test "changeset with invalid attributes" do
    changeset = Setup.changeset(%Setup{}, @invalid_attrs)
    refute changeset.valid?
  end
end
