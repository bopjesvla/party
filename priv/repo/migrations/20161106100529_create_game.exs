defmodule Mafia.Repo.Migrations.CreateGame do
  use Ecto.Migration

  def change do
    create table(:games) do
      add :name, :string
      add :setup_id, references(:setups, on_delete: :nothing)
      add :status, :string
      add :speed, :integer

      timestamps()
    end
    create index(:games, [:setup_id])

  end
end
