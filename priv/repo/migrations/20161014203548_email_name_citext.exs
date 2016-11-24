defmodule Mafia.Repo.Migrations.EmailNameCitext do
  use Ecto.Migration

  def change do
    execute "CREATE EXTENSION IF NOT EXISTS citext"
    alter table(:users) do
      modify :name, :citext
      modify :email, :citext
    end
  end
end
