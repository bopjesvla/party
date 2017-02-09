defmodule Mafia.SetupTeam do
  use Mafia.Web, :model

  schema "setup_teams" do
    field :team, :string
    field :player, :integer
    belongs_to :setup, Mafia.Setup

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:team, :player])
    |> validate_required([:team, :player])
  end
end
