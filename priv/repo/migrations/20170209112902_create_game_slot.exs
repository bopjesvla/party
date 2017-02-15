defmodule Mafia.Repo.Migrations.CreateGameSlot do
  use Ecto.Migration

  def change do
    create table(:game_slots) do
      add :status, :text
      add :game_id, references(:games, on_delete: :nothing)
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:game_slots, [:game_id, :user_id],
      name: "unique_player",
      where: "game_slots.status = 'playing' or game_slots.status = 'replaced'")
    create index(:game_slots, [:user_id])
  end
end
