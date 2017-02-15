defmodule Mafia.GameServer do
  use GenServer
  alias Mafia.{Repo,Game,Channel,MeetChannel,GameChannel}

  # management

  defp to_id(str) when is_binary(str) do
    String.to_integer str
  end
  defp to_id(id), do: id

  defp via_tuple(id) do
    {:via, Registry, {:game_registry, to_id(id)}}
  end

  def start_link(%{id: id} = game) do
    game = game
    |> Repo.preload([setup: [:teams, :roles], players: []])

    GenServer.start_link(__MODULE__, game, name: via_tuple(id))
  end

  # API

  def query(id, terms) do
    id
    |> via_tuple
    |> GenServer.call({:query, terms})
  end

  def query!(id, terms) do
    {:succeed, res} = query(id, terms)
    res
  end

  def stop(id) do
    id
    |> via_tuple
    |> GenServer.stop
  end

  # callbacks

  def init(game) do
    db = game_db()
    |> load_setup(game.setup)
    |> load_players(game.players)

    {{:succeed, _}, db} = :erlog.prove(:next_phase, db)

    {:ok, %{db: db, game: game}}
  end

  def handle_call({:query, terms}, _, state) do
    {res, db} = :erlog.prove(terms, state.db)

    {:reply, res, %{state | db: db}}
  end

  def handle_info({:create_channel, channel}, state) do
    Repo.insert!(%Channel{game_id: state.game.id, name: channel, type: "meet"})
    {:noreply, state}
  end

  def handle_info({:join, player, channel}, state) do
    %{user_id: user} = Repo.get!(Mafia.GamePlayer, player)
    MeetChannel.new_message(channel, "join", user, nil)
    {:noreply, state}
  end
  def handle_info({:next_phase, at}, %{id: id} = state) do
    Mafia.Endpoint.broadcast!("game:#{id}", "info", %{'phase.next': at})
    {:noreply, state}
  end
  def handle_info({:leave, channel}, state) do
    Mafia.Endpoint.broadcast! "meet:#{channel}", "leave", %{who: :all}
    {:noreply, state}
  end
  def handle_info(:do_next_phase, state) do
    {{:succeed, _}, db} = :erlog.prove(:next_phase, state.db)
    {:noreply, %{state | db: db}}
  end
  def handle_info({:new_phase, phase}, %{id: id} = state) do
    Mafia.Endpoint.broadcast!("game:#{id}", "info", %{phase: phase})
    {:noreply, state}
  end
  def handle_info({:vote, player, channel, act, targets}, state) do
    message = %{act: act, targets: targets} |> Poison.encode!
    %{user_id: user} = Repo.get!(Mafia.GamePlayer, player)
    MeetChannel.new_message(channel, "vote", user, message)
    {:noreply, state}
    end
  def handle_info({:message, player, message}, %{id: id} = state) do
    %{user_id: user} = Repo.get!(Mafia.GamePlayer, player)
    GameChannel.new_message(id, "sys", user, message)
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
      target = if r.type == "alignment", do: r.str, else: r.nr
      role = {:',', Enum.map(r.mods, &atom/1), atom(r.role)}
      fact = {:setup_role, atom(r.type), target, role}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    phases = Enum.map setup.phases, &String.to_existing_atom/1
    {{:succeed, _}, db} = :erlog.prove({:asserta, {:setup_phases, phases}}, db)
    db
  end

  def load_players(db, players) do
    Enum.reduce players, db, fn (player, db) ->
      {{:succeed, _}, db} = :erlog.prove({:asserta, {:player, player.id}}, db)
      db
    end
  end
end
