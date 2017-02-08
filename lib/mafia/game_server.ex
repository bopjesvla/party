alias Mafia.{Repo,Game,Channel,MeetChannel,GameChannel}

defmodule Mafia.GameServer do
  use GenServer

  # management

  defp via_tuple(name) do
    {:via, Registry, {:game_registry, name}}
  end

  def start_link({name, _, _, _} = args) do
    GenServer.start_link(__MODULE__, args, name: via_tuple(name))
  end

  # API

  def query(name, terms) do
    name
    |> via_tuple
    |> GenServer.call({:query, terms})
  end

  def query!(name, terms) do
    {:succeed, res} = query(name, terms)
    res
  end

  def stop(name) do
    name
    |> via_tuple
    |> GenServer.stop
  end

  # callbacks

  def init({name, user, setup, facts}) do
    db = game_db()
    |> load_setup(setup)

    db = Enum.reduce facts, db, fn (fact, db) ->
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    {{:succeed, _}, db} = :erlog.prove({:create_channel, :signups, nil, {:_}}, db)
    {{:succeed, _}, db} = :erlog.prove({:join, user}, db)
    {:ok, %{db: db, name: name}}
  end

  def handle_call({:query, terms}, _, state) do
    {res, db} = :erlog.prove(terms, state.db)

    {:reply, res, %{state | db: db}}
  end

  def handle_info({:create_channel, channel}, %{name: name} = state) do
    %{id: game_id} = Repo.get_by(Game, name: name)
    Repo.insert!(%Channel{game_id: game_id, name: channel, type: "m"})
    {:noreply, state}
  end

  def handle_info({:join, user, channel}, state) do
    MeetChannel.external_message(channel, "join", user, nil)
    {:noreply, state}
  end
  def handle_info({:next_phase, at}, %{name: name} = state) do
    Mafia.Endpoint.broadcast!("game:#{name}", "info", %{'phase.next': at})
    {:noreply, state}
  end
  def handle_info({:leave, who, channel}, state) do
    message = %{who: who} |> Poison.encode!
    MeetChannel.external_message channel, "leave", nil, message
    {:noreply, state}
  end
  def handle_info(:do_next_phase, state) do
    {{:succeed, _}, db} = :erlog.prove(:next_phase, state.db)
    {:noreply, %{state | db: db}}
  end
  def handle_info({:new_phase, phase}, %{name: name} = state) do
    Mafia.Endpoint.broadcast!("game:#{name}", "info", %{phase: phase})
    {:noreply, state}
  end
  def handle_info({:vote, user, channel, act, targets}, state) do
    message = %{act: act, targets: targets} |> Poison.encode!
    MeetChannel.external_message(channel, "vote", user, message)
    {:noreply, state}
  end
  def handle_info({:message, user, message}, %{name: name} = state) do
    GameChannel.external_message(name, "sys", user, message)
    {:noreply, state}
  end
  def handle_info(s, _) do
    raise "shit" ++ s
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
      fact = {:setup_alignment, t.player, t.team}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    db = Enum.reduce setup.roles, db, fn (r, db) ->
      target = if r.type == "alignment", do: r.team, else: r.player
      role = {:',', Enum.map(r.mods, &atom/1), atom(r.role)}
      fact = {:setup_role, atom(r.type), target, role}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    phases = Enum.map setup.phases, &String.to_existing_atom/1
    {{:succeed, _}, db} = :erlog.prove({:asserta, {:setup_phases, phases}}, db)
    db
  end
end
