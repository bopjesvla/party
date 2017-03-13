defmodule Mafia.DescriptionController do
  use Mafia.Web, :controller

  def show(conn, %{"type" => type, "name" => name}) do
    render conn, "#{type}/#{name}.html", layout: false
  end
end
