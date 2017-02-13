defmodule Mafia.SetupView do
  use Mafia.Web, :view

  def render("index.json", %{setup: setup}) do
    %{data: render_many(setup, Mafia.SetupView, "setup.json")}
  end

  def render("show.json", %{setup: setup}) do
    %{data: render_one(setup, Mafia.SetupView, "setup.json")}
  end

  def render("setup.json", %{setup: setup}) do
    %{id: setup.id,
      size: setup.size,
      phases: setup.phases,
      user_id: setup.user_id}
  end
end
