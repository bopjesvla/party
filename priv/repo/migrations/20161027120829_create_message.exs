defmodule Mafia.Repo.Migrations.CreateMessage do
  use Ecto.Migration

  def change do
    create table(:messages) do
      add :type, :text
      add :msg, :text
      add :channel_id, references(:channels, on_delete: :nothing)
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create index(:messages, [:channel_id])
    create index(:messages, [:user_id])

  end
end
