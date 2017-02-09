defmodule Mafia.Repo.Migrations.CreateSetupRole do
  use Ecto.Migration

  def change do
    create table(:setup_roles) do
      add :type, :string
      add :nr, :integer
      add :str, :string
      add :mods, {:array, :string}
      add :role, :string
      add :setup, references(:setups, on_delete: :nothing)

      timestamps()
    end
    create index(:setup_roles, [:setup])

  end
end
