defmodule Mafia.SetupTeamTest do
  use Mafia.ModelCase

  alias Mafia.SetupTeam

  @valid_attrs %{player: 42, team: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = SetupTeam.changeset(%SetupTeam{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = SetupTeam.changeset(%SetupTeam{}, @invalid_attrs)
    refute changeset.valid?
  end
end
