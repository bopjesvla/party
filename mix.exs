defmodule Mafia.Mixfile do
  use Mix.Project

  def project do
    [app: :mafia,
     version: "0.0.1-#{commitcount()}",
     elixir: "~> 1.2",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     aliases: aliases(),
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [mod: {Mafia, []},
     applications: [:phoenix, :phoenix_pubsub, :phoenix_html, :cowboy, :logger, :gettext,
                    :phoenix_ecto, :postgrex, :coherence, :httpoison,
                    :erlog, :gen_smtp, :phoenix_markdown, :porta,
                    :edeliver # keep at end
                    ]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [{:phoenix, "~> 1.2.1"},
     {:phoenix_pubsub, "~> 1.0"},
     {:phoenix_ecto, "~> 3.0"},
     {:postgrex, ">= 0.13.1"},
     {:phoenix_html, "~> 2.6"},
     {:phoenix_live_reload, "~> 1.0", only: :dev},
     {:wallaby, "~> 0.14", only: :test},
     {:gettext, "~> 0.11"},
     {:coherence, "~> 0.3"},
     {:cowboy, "~> 1.0"},
     {:httpoison, "~> 0.10.0"},
     {:erlog, github: "rvirding/erlog", branch: "develop"},
    {:porta, "~> 0.1.0"},
    {:phoenix_markdown, "~> 0.1.4"},
    {:distillery, "~> 1.0"},
    {:edeliver, "~> 1.4.2"}
   ]
  end

  defp commitcount() do
    {result, _exit_code} = System.cmd("git", ["rev-list", "--count", "HEAD"])

    String.strip result
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    ["ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
     "ecto.reset": ["ecto.drop", "ecto.setup"],
     "test": fn
       ["prolog" | _] ->
          Mix.Task.run "test", ~w(--only prolog)
       ["browser" | _] ->
         Mix.Task.run "test", ~w(--only browser)
       args ->
         Mix.Task.run "ecto.reset", []
         Mix.Task.run "test", ["--exclude", "browser" | args]
       end,
     "install": ["cmd swipl -f install.pl -g install -t halt"],
     "game.serve": "cmd swipl game_server.pl"]
  end
end
