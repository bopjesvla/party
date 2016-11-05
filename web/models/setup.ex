defmodule Mafia.Setup do
  use Mafia.Web, :model

  schema "setups" do
    field :name, :string
    belongs_to :user, Mafia.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name])
    |> validate_required([:name])
    |> unique_constraint(:name)
  end
end
