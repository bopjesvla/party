defmodule Mafia.Room do
  use Mafia.Web, :model

  schema "rooms" do
    field :type, :string
    field :name, :string
    field :archived, :boolean, default: false
    belongs_to :creator, Mafia.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:type, :name, :archived])
    |> validate_required([:type, :name, :archived])
    |> unique_constraint(:name)
  end
end
