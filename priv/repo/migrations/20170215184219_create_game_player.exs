defmodule Mafia.Repo.Migrations.CreateGamePlayer do
  use Ecto.Migration

  def change do
    create table(:game_players) do
      add :status, :string
      add :user_id, references(:users, on_delete: :nothing)
      add :game_slot_id, references(:game_slots, on_delete: :nothing)

      timestamps()
    end
    create index(:game_players, [:user_id])
    create index(:game_players, [:game_slot_id])

    create unique_index(:game_players, [:game_slot_id], where: "status = 'playing'")
  end
end
