defmodule Mafia.GamePlayer do
  use Mafia.Web, :model

  schema "game_players" do
    field :status, :string
    belongs_to :user, Mafia.User
    belongs_to :game_slot, Mafia.GameSlot

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:status, :game_slot_id])
    |> validate_required([:status, :game_slot_id])
  end
end
