defmodule Mafia.RoomMessage do
  use Mafia.Web, :model

  schema "room_messages" do
    field :msg, :string
    belongs_to :user, Mafia.User
    belongs_to :room, Mafia.Room

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:msg])
    |> validate_required([:msg])
  end
end
