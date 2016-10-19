defmodule Mafia.Repo.Migrations.CreateRoom do
  use Ecto.Migration

  def change do
    create table(:rooms) do
      add :type, :text
      add :name, :text
      add :archived, :boolean, default: false, null: false
      add :creator_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:rooms, [:name])
    create index(:rooms, [:creator_id])

  end
end
