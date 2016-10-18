defmodule Mafia.Repo.Migrations.CreateRoom do
  use Ecto.Migration

  def change do
    create table(:rooms) do
      add :type, :text
      add :name, :text
      add :archived, :boolean, default: false, null: false
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:rooms, [:name])
    create index(:rooms, [:user_id])

  end
end
