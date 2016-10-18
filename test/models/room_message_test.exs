defmodule Mafia.RoomMessageTest do
  use Mafia.ModelCase

  alias Mafia.RoomMessage

  @valid_attrs %{msg: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = RoomMessage.changeset(%RoomMessage{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = RoomMessage.changeset(%RoomMessage{}, @invalid_attrs)
    refute changeset.valid?
  end
end
