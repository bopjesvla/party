defmodule Mafia.Repo.Migrations.CreateChannel do
  use Ecto.Migration

  def change do
    create table(:channels) do
      add :name, :citext
      add :type, :text
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:channels, [:name])
    create index(:channels, [:user_id])

  end
end
