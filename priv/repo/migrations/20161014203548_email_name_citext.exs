defmodule Mafia.Repo.Migrations.EmailNameCitext do
  use Ecto.Migration

  def change do
    alter table(:users) do
      modify :name, :citext
      modify :email, :citext
    end
  end
end
