defmodule Mafia.Repo.Migrations.CreateRoomMessage do
  use Ecto.Migration

  def change do
    create table(:room_messages) do
      add :msg, :text
      add :user_id, references(:users, on_delete: :nothing)
      add :room_id, references(:rooms, on_delete: :nothing)

      timestamps()
    end
    create index(:room_messages, [:user_id])
    create index(:room_messages, [:room_id])

  end
end
