defmodule Mafia.Repo.Migrations.CreateGameSlot do
  use Ecto.Migration

  def change do
    create table(:game_slots) do
      add :game_id, references(:games, on_delete: :nothing)
      add :player, :integer

      timestamps()
    end
    create index(:game_slots, [:game_id])
  end
end
