defmodule Mafia.Repo.Migrations.AddActiveToChannel do
  use Ecto.Migration

  def change do
    alter table :channels do
      add :active_subchannel_id, references(:subchannels, on_delete: :nothing)
    end

    create index(:channels, :active_subchannel_id)
  end
end
