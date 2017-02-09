defmodule Mafia.Setup do
  use Mafia.Web, :model

  schema "setups" do
    field :name, :string
    field :size, :integer
    field :phases, {:array, :string}
    has_many :roles, Mafia.SetupRole
    has_many :teams, Mafia.SetupTeam
    belongs_to :user, Mafia.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :size, :phases])
    |> cast_assoc(:roles)
    |> cast_assoc(:teams)
    |> validate_required([:name, :size, :phases, :roles, :teams])
    |> unique_constraint(:name)
  end
end
