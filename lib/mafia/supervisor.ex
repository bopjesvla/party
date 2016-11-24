defmodule Mafia.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: :game_supervisor)
  end

  def start_game(args) do
    Supervisor.start_child(:game_supervisor, [args])
  end

  def init(_) do
    children = [
      worker(Mafia.GameServer, [])
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end

Mafia.Supervisor.start_link
