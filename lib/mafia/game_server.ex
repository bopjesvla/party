alias Mafia.{Repo,Game,Channel,Message}
alias Mafia.GameServer.{Player, Role}

defmodule Mafia.GameServer do
  use GenServer

  defstruct id: 1, name: nil, phase: "signups", phase_index: -1, creator: 1, users: [], players: [], setup: 1, size: 3, recovery: false, timers: %{}, phase_end: :infinity

  defp via_tuple(name) do
    {:via, :gproc, {:n, :l, {:game, name}}}
  end

  def start_link({name, user, opts}) do
    GenServer.start_link(__MODULE__, {name, user, opts}, name: via_tuple(name))
  end

  def init(args) do
    if false do
      recover(args)
    else
      create(args)
    end
  end

  def reply({s, m}), do: {:reply, s, m}
  def reply(m), do: {:reply, :ok, m}

  def message(msg, %{recovery: true}), do: nil
  def message(msg, g) do
    %{inserted_at: inserted_at} = Repo.insert! Message.changeset(%Message{channel_id: g.name}, %{type: "s", msg: msg, user_id: -1})
    Mafia.EndPoint.broadcast! "game:" <> g.name, "new:msg", %{msg: msg, t: "s", ts: inserted_at}
  end

  def create({name, user, opts}) do
    channel = Repo.insert! %Channel{user_id: user, type: "g", name: name}
    game = Repo.insert! %Game{user_id: user, seed: 5, setup: nil}
    g = %__MODULE__{id: game.id, creator: user, name: name}
    {:ok, g}
  end

  def join(name, user) do
    GenServer.call(via_tuple(name), {:join, user})
  end

  def is_playing(user, g), do: Enum.any? g.players, &(&1.user == user) 

  def add_player(user, g) do
    cond do
      is_playing(user, g) ->
        {:error, "Already playing"}
      length(g.players) < g.size ->
        new_player = %Player{user: user}
        Kernel.update_in(g.players, &([new_player | &1]))
        |> reply
      i = Enum.find_index g.players, &(&1.replacable == true) ->
        Kernel.update_in g, [Access.key!(:players), Access.at(i)], &(%{&1 | user: user, replacable: false})
        |> reply
    end
  end

  def recover({name, user, opts}) do
    nil
  end

  def handle_call({:join, user}, _, g) do
    cond do
      is_playing(user, g) ->
        reply(g)
      true ->
        add_player(user, g) |> reply
    end
  end

  def handle_call({:leave, user}, _, g) do
    
  end

  def handle_call({:action, action}, g) do
    {:reply, :ok, g}
  end

  def start_game(g = %{phase: "signups"}) do
    next_phase(g)
  end

  def next_phase(g) do
    index = (g.phase_index + 1) |> rem g.size
    phase = String.slice g.setup.phases, g.setup.phase_index, 1
    %{g | phase_index: index, phase: phase}
  end
end
