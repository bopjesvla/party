defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    belongs_to :setup, Mafia.Setup
    has_many :channel, Mafia.Channel
    field :pengine, :string
    field :status, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:pengine, :status])
    |> validate_required([:pengine, :status])
  end
end
