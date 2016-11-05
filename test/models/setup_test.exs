defmodule Mafia.SetupTest do
  use Mafia.ModelCase

  alias Mafia.Setup

  @valid_attrs %{name: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Setup.changeset(%Setup{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Setup.changeset(%Setup{}, @invalid_attrs)
    refute changeset.valid?
  end
end
