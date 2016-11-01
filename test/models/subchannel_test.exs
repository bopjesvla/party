defmodule Mafia.SubchannelTest do
  use Mafia.ModelCase

  alias Mafia.Subchannel

  @valid_attrs %{}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Subchannel.changeset(%Subchannel{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Subchannel.changeset(%Subchannel{}, @invalid_attrs)
    refute changeset.valid?
  end
end
