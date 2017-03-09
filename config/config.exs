# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :mafia,
  ecto_repos: [Mafia.Repo],
  signups_countdown: 10000

# Configures the endpoint
config :mafia, Mafia.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "RpVal5xzfIpeVggggU5DB0l4WicxjUMSq+udMVtLqJYw5I4mplI9ZN2ZUgY76xPz",
  render_errors: [view: Mafia.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Mafia.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :phoenix, :template_engines,
  md: PhoenixMarkdown.Engine

# %% Coherence Configuration %%   Don't remove this line
config :coherence,
  user_schema: Mafia.User,
  login_field: :name,
  repo: Mafia.Repo,
  module: Mafia,
  logged_out_url: "/home",
  email_from: {"Bob", "negenentwintig@hotmail.com"},
  opts: [:authenticatable, :recoverable, :lockable, :trackable, :unlockable_with_token, :registerable]

config :coherence, Mafia.Coherence.Mailer,
  adapter: Swoosh.Adapters.Sendgrid,
  api_key: "your api key here"
# %% End Coherence Configuration %%

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
