defmodule Mafia.Channel do
  use Mafia.Web, :model

  schema "channels" do
    field :name, :string
    field :type, :string
    belongs_to :user, Mafia.User
    has_many :messages, Mafia.Message

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
    |> foreign_key_constraint(:user)
  end
end
