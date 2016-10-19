defmodule Mafia.HomeController do
  use Mafia.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
