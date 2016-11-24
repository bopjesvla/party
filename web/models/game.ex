defmodule Mafia.Game do
  use Mafia.Web, :model

  schema "games" do
    field :seed, :integer
    belongs_to :user, Mafia.User
    belongs_to :setup, Mafia.Setup
    belongs_to :channel, Mafia.Channel

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:seed])
    |> validate_required([:seed])
  end
end
