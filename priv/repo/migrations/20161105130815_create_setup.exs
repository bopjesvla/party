defmodule Mafia.Repo.Migrations.CreateSetup do
  use Ecto.Migration

  def change do
    create table(:setups) do
      add :name, :citext
      add :size, :integer
      add :phases, {:array, :string}
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end
    create unique_index(:setups, [:name])
    create index(:setups, [:user_id])

  end
end
