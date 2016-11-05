defmodule Mafia.Repo.Migrations.CreateGame do
  use Ecto.Migration

  def change do
    create table(:games) do
      add :name, :text
      add :seed, :integer
      add :user_id, references(:users, on_delete: :nothing)
      add :setup, references(:setups, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:games, [:name])
    create index(:games, [:user_id])
    create index(:games, [:setup])

  end
end
