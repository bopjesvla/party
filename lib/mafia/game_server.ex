alias Mafia.{Repo,Game,Channel,Message}
alias Mafia.GameServer.{Player, Role}

defmodule Mafia.GameServer do
  use GenServer

  # management

  defp via_tuple(name) do
    {:via, Registry, {:game_registry, name}}
  end

  def start_link({name, user, setup}) do
    GenServer.start_link(__MODULE__, {name, user, setup}, name: via_tuple(name))
  end
  
  # API
  
  def prove(name, terms) do
    name
    |> via_tuple
    |> GenServer.call({:prove, terms})
  end

  def stop(name) do
    name
    |> via_tuple
    |> GenServer.stop
  end


  # callbacks
  
  def init({name, user, setup}) do
    db = game_db
    |> load_setup(setup)
    
    {{:succeed, _}, db} = :erlog.prove({:join, user}, db)
    {:ok, %{db: db}}
  end
  
  def handle_call({:prove, terms}, _, state) do
    {res, db} = :erlog.prove(terms, state.db)
    {:reply, res, %{state | db: db}}
  end
  
  # helpers
  
  defdelegate atom(string), to: String, as: :to_existing_atom
  
  def game_db do
    {:ok, db} = :erlog.new
    Enum.reduce ~w(mafia.pl roles.pl actions.pl resolve.pl utils.pl)c, db, fn (file, db) -> 
      {:ok, db} = :erlog.consult('prolog/' ++ file, db)
      db
    end
  end
  
  def load_setup(db, setup) do
    db = Enum.reduce setup.teams, db, fn (t, db) ->
      fact = {:setup_alignment, t.player, atom(t.team)}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end
    
    Enum.reduce setup.roles, db, fn (r, db) ->
      target = if r.type == "alignment", do: atom(r.team), else: r.player
      role = {:',', Enum.map(r.mods, &atom/1), atom(r.role)}
      fact = {:setup_role, atom(r.type), target, role}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end
  end
end
