defmodule Mafia.Repo.Migrations.CreateMessage do
  use Ecto.Migration

  def change do
    create table(:messages) do
      add :type, :text
      add :msg, :text
      add :subchannel_id, references(:subchannels, on_delete: :nothing)
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create index(:messages, [:subchannel_id])
    create index(:messages, [:user_id])

  end
end
