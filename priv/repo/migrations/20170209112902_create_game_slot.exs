defmodule Mafia.Repo.Migrations.CreateGameSlot do
  use Ecto.Migration

  def change do
    create table(:game_slots) do
      add :game_id, references(:games, on_delete: :nothing), null: false
      add :setup_player, :integer
    end
    create index(:game_slots, [:game_id])
  end
end
