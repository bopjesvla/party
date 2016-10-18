defmodule Mafia.Router do
  use Mafia.Web, :router
  use Coherence.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug Coherence.Authentication.Session
  end

  pipeline :protected do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug Coherence.Authentication.Session, protected: true
    plug :put_user_token
  end

  defp put_user_token(conn, _) do
    if current_user = conn.assigns[:current_user] do
      token = Phoenix.Token.sign(conn, "user socket", current_user.id)
      assign(conn, :user_token, token)
    else
      conn
    end
  end

  pipeline :default_layout do
    plug :put_layout, {Mafia.LayoutView, "app.html"}
  end

  scope "/", Mafia do
    pipe_through :browser
    pipe_through :default_layout
    get "/", Coherence.RegistrationController, :new
    get "/sign-in", Coherence.SessionController, :new
    get "/forgot-password", Coherence.PasswordController, :new
    coherence_routes
  end

  scope "/", Mafia do
    pipe_through :protected
    pipe_through :default_layout
    coherence_routes :protected
  end

  scope "/", Mafia do
    pipe_through :browser # Use the default browser stack
  end

  scope "/", Mafia do
    pipe_through :protected
    pipe_through :default_layout

    get "/home", HomeController, :index
    get "/home/*path", HomeController, :index
    # add protected resources below
    #resources "/privates", Mafia.PrivateController
  end

  # Other scopes may use custom stacks.
  # scope "/api", Mafia do
  #   pipe_through :api
  # end
end
