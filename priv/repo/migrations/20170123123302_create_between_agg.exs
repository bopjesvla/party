defmodule Mafia.Repo.Migrations.CreateBetweenAgg do
  use Ecto.Migration

  def change do
    Path.join(__DIR__, "between_agg.sql")
    |> File.read!
    |> String.split("-----")
    |> Enum.each(&execute/1)
  end
end
