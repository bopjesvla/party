defmodule Mafia.GameSlot do
  use Mafia.Web, :model

  schema "game_slots" do
    belongs_to :game, Mafia.Game
    has_many :game_players, Mafia.GamePlayer

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [])
  end
end
