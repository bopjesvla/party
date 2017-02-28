defmodule Mafia.GameSlot do
  use Mafia.Web, :model

  schema "game_slots" do
    belongs_to :game, Mafia.Game
    field :setup_player, :integer
    has_many :game_players, Mafia.GamePlayer
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:setup_player])
  end
end
