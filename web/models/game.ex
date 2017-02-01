defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    belongs_to :setup, Mafia.Setup
    has_many :channels, Mafia.Channel
    field :name, :string
    field :status, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :status])
    |> validate_required([:name, :status])
  end
end
