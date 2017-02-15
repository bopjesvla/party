defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    belongs_to :setup, Mafia.Setup
    has_many :channels, Mafia.Channel
    has_many :slots, Mafia.GameSlot
    has_many :players, through: [:slots, :game_players]
    field :name, :string
    field :status, :string
    field :speed, :integer

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :speed])
    |> validate_required([:speed])
    |> validate_number(:speed, less_than: 20000, greater_than: 0.1)
  end
end
