defmodule Mafia do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec

    pg_config = Application.get_env(:mafia, Mafia.Repo)

    # Define workers and child supervisors to be supervised
    children = [
      # Start the Ecto repository
      supervisor(Mafia.Repo, []),
      # Start the endpoint when the application starts
      supervisor(Mafia.Endpoint, []),
      # Start your own worker by calling: Mafia.Worker.start_link(arg1, arg2, arg3)
      # worker(Mafia.Worker, [arg1, arg2, arg3]),
      supervisor(Mafia.GameSupervisor, []),
      supervisor(Registry, [:unique, :game_registry]),
      supervisor(Registry, [:unique, :timer_registry], id: :timer_registry),
      worker(Mafia.QueueBroadcaster, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Mafia.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Mafia.Endpoint.config_change(changed, removed)
    :ok
  end
end
