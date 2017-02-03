alias Mafia.{Repo,Game,Channel,MeetChannel,GameChannel}

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
  
  def query(name, terms) do
    name
    |> via_tuple
    |> GenServer.call({:query, terms})
  end

  def stop(name) do
    name
    |> via_tuple
    |> GenServer.stop
  end
  
  def handle_message(game, {:create_channel, channel}) do
    %{id: game_id} = Repo.get_by(Game, name: game)
    Repo.insert!(%Channel{game_id: game_id, name: channel, type: "m"})
  end
    
  def handle_message(game, {:join, user, channel}), do: MeetChannel.external_message(channel, "join", user, nil)
  def handle_message(game, {:next_phase, delay}) do
    # __MODULE__ for test mocking
    MeetChannel.update_info(:next_phase, delay)
  end
  def handle_message(game, {"leave", [who, channel]}), do: Mafia.Endpoint.broadcast! "meet:#{channel}", "leave", %{who: who}

  # callbacks
  
  def init({name, user, setup}) do
    db = game_db
    |> load_setup(setup)
    
    {{:succeed, _}, db} = :erlog.prove({:create_channel, :signups, nil, {:_}}, db)   
    {{:succeed, _}, db} = :erlog.prove({:join, user}, db)
    {:ok, %{db: db}}
  end
  
  def handle_call({:query, terms}, _, state) do
    {res, db, messages} = query_and_flush(state.db, terms)
    {res, db} = :erlog.prove(terms, db)

    {:reply, res, %{state | db: db}}
  end
  
  # helpers
  
  defdelegate atom(string), to: String, as: :to_existing_atom
  
  def game_db do
    {:ok, db} = :erlog.new
    Enum.reduce ~w(mafia.pl roles.pl actions.pl resolve.pl utils.pl)c, db, fn (file, db) ->
      case :erlog.consult('prolog/' ++ file, db) do
        {:ok, db} -> db
        {:error, err} -> raise {:error, file, err}
      end
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
  
  def query_and_flush(db, terms) do
    {res, db} = :erlog.prove(terms, db)
    {{:succeed, [messages: messages]}, flushed_db} = :erlog.prove({:flush, {:messages}}, db)
    {res, flushed_db, messages}
  end
end
