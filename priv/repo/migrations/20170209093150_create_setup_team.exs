defmodule Mafia.Repo.Migrations.CreateSetupTeam do
  use Ecto.Migration

  def change do
    create table(:setup_teams) do
      add :team, :string
      add :player, :integer
      add :setup_id, references(:setups, on_delete: :nothing)

      timestamps()
    end
    create index(:setup_teams, [:setup_id])

  end
end
