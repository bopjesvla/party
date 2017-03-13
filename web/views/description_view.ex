defmodule Mafia.DescriptionView do
  use Phoenix.View, root: "", path: "descriptions", pattern: "**/*"

  def template_not_found(_template, _assigns) do
    "No description found"
  end
end
