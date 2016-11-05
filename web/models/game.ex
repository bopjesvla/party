defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    field :name, :string
    field :seed, :integer
    belongs_to :user, Mafia.User
    belongs_to :setup, Mafia.Setup

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :seed])
    |> validate_required([:name, :seed])
    |> unique_constraint(:name)
  end
end
