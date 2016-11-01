defmodule Mafia.Channel do
  use Mafia.Web, :model

  schema "channels" do
    field :name, :string
    field :type, :string
    belongs_to :user, Mafia.User
    has_many :subchannels, Mafia.Subchannel
    belongs_to :active_subchannel, Mafia.Subchannel

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :type])
    |> validate_required([:name, :type])
    |> unique_constraint(:name)
  end
end
