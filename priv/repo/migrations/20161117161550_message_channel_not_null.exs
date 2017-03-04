defmodule Mafia.Repo.Migrations.MessageChannelNotNull do
  use Ecto.Migration

  def change do
    alter table(:messages) do
      modify :channel_id, :integer, null: false
    end
    alter table(:setups) do
      modify :user_id, :integer, null: false
    end
  end
end
