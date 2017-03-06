defmodule Mafia.Repo.Migrations.AddGameColumnToChannel do
  use Ecto.Migration

  def change do
    alter table(:channels) do
      add :game_id, references(:games, on_delete: :nothing)
    end

    create index(:channels, [:game_id])    
  end
end
