defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    belongs_to :setup, Mafia.Setup
    belongs_to :channel, Mafia.Channel
    field :pengine, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:pengine])
    |> validate_required([:pengine])
  end
end
