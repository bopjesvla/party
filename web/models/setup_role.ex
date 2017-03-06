defmodule Mafia.SetupRole do
  use Mafia.Web, :model

  schema "setup_roles" do
    field :type, :string
    field :nr, :integer
    field :str, :string
    field :mods, {:array, :string}
    field :role, :string
    belongs_to :setup, Mafia.Setup

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:type, :nr, :str, :mods, :role])
    |> validate_required([:type, :mods, :role])
  end
end
