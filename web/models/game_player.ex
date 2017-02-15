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
    |> cast(params, [:status])
    |> validate_required([:status])
  end
end
