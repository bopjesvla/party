defmodule Mafia.Message do
  use Mafia.Web, :model

  schema "messages" do
    field :type, :string
    field :msg, :string
    belongs_to :channel, Mafia.Channel
    belongs_to :user, Mafia.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:type, :msg])
    |> validate_required([:type, :msg])
  end
end
