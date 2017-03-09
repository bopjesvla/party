defmodule Mafia.RoleDescriptionController do
  use Mafia.Web, :controller

  def show(conn, %{"role" => role}) do
    render conn, "#{role}.html", layout: false
  end
end
