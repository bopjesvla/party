defmodule Mafia.Repo.Migrations.CreateGame do
  use Ecto.Migration

  def change do
    create table(:games) do
      add :pengine, :string
      add :channel_id, references(:channels, on_delete: :nothing)
      add :setup_id, references(:setups, on_delete: :nothing)

      timestamps()
    end
    create index(:games, [:setup_id])
    create index(:games, [:channel_id])

  end
end
