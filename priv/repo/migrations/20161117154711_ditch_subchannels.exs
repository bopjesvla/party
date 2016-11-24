defmodule Mafia.Repo.Migrations.DitchSubchannels do
  use Ecto.Migration

  def change do
    alter table(:channels) do
      remove :active_subchannel_id
    end
    alter table(:messages) do
      remove :subchannel_id
      add :channel_id, references(:channels)
    end
    drop table(:subchannels)
  end
end
