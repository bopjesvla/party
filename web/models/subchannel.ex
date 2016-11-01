defmodule Mafia.Subchannel do
  use Mafia.Web, :model

  schema "subchannels" do
    belongs_to :channel, Mafia.Channel

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [])
    |> validate_required([])
  end
end
