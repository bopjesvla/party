defmodule Mafia.Repo.Migrations.CreateSubchannel do
  use Ecto.Migration

  def change do
    create table(:subchannels) do
      add :channel_id, references(:channels, on_delete: :nothing)

      timestamps()
    end
    create index(:subchannels, [:channel_id])

  end
end
