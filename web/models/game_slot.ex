defmodule Mafia.GameSlot do
  use Mafia.Web, :model

  schema "game_players" do
    field :status, :string
    belongs_to :game, Mafia.Game
    belongs_to :user, Mafia.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:status])
    |> validate_required([:status])
    |> unique_constraint(:user,
      name: "unique_player", message: "You already played in this game")
  end
end
