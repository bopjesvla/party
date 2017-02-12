defmodule Mafia.GameInfoView do
  use Mafia.Web, :view
  import Access

  def render("game_info.json", game) do
    game = game
    |> Map.drop(~w(inserted_at updated_at __meta__)a)
    |> update_in([key!(:roles), all], &Map.drop(&1, ~w(inserted_at updated_at __meta__)a))
    |> update_in([key!(:teams), all], &Map.drop(&1, ~w(inserted_at updated_at __meta__)a))
    |> update_in([key!(:players), all, key!(:user)], &Map.take(&1, ~w(name)a))
  end
end
