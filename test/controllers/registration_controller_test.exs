defmodule Mafia.RegistrationControllerTest do
  use Mafia.ConnCase

  @user_data %{registration: %{email: "a@b", name: "q", password: "1234", password_confirmation: "1234"}}

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "Register"
  end

  test "unique name", %{conn: conn} do
    conn = post conn, "/registrations", @user_data
    assert html_response(conn, 302)
  end
end
