defmodule Mafia.Repo.Migrations.CreateGamePlayer do
  use Ecto.Migration

  def change do
    create table(:game_players) do
      add :status, :text
      add :game_id, references(:games, on_delete: :nothing)
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:game_players, [:game_id, :user_id],
      name: "unique_player",
      where: "game_players.status = 'playing' or game_players.status = 'replaced'")
    create index(:game_players, [:user_id])
  end
end
