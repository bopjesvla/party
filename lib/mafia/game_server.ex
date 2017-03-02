defmodule Mafia.GameServer do
  use GenServer
  alias Mafia.{Repo,Channel,MeetChannel,GameChannel}
  import Ecto.Query

  @game_db_files ~w(mafia.pl roles.pl actions.pl resolve.pl utils.pl end.pl)c

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
    |> Repo.preload([setup: [:teams, :roles], slots: []])

    GenServer.start_link(__MODULE__, game, name: via_tuple(id))
  end

  def start(%{id: id} = game) do
    game = game
    |> Repo.preload([setup: [:teams, :roles], slots: []])

    GenServer.start(__MODULE__, game, name: via_tuple(id))
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

  def crash(id) do
    id
    |> via_tuple
    |> GenServer.stop
  end

  # callbacks

  def init(game) do
    db = game_db()
    |> load_setup(game.setup)
    |> load_slots(game.slots)

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

  def handle_info({:join, slot, channel}, state) do
    %{user_id: user} = Mafia.Queries.player_for_slot(slot)
    MeetChannel.new_message("meet:#{channel}", "join", user, nil)
    {:noreply, state}
  end
  def handle_info({:next_phase, at}, %{game: %{id: id}} = state) do
    Mafia.Endpoint.broadcast!("game:#{id}", "info", %{'phase.next': at})
    {:noreply, state}
  end
  def handle_info({:leave, channel}, state) do
    Mafia.Endpoint.broadcast! "meet:#{channel}", "leave", %{who: :all}
    {:noreply, state}
  end
  def handle_info({:leave, who, channel}, state) do
    %{user_id: user} = Repo.get_by!(Mafia.GamePlayer, game_slot_id: who)
    MeetChannel.new_message("meet:#{channel}", "kick", user, nil)
    Mafia.Endpoint.broadcast! "meet:#{channel}", "leave", %{who: who}
    {:noreply, state}
  end
  def handle_info(:do_next_phase, state) do
    {{:succeed, _}, db} = :erlog.prove(:next_phase, state.db)
    {:noreply, %{state | db: db}}
  end
  def handle_info({:new_phase, info}, %{game: %{id: id}} = state) do
    GameChannel.new_message(id, "phase", nil, "#{info[:name]} #{info[:number]}")
    {:noreply, state}
  end
  def handle_info({:vote, slot, channel, act, targets}, state) do
    message = %{act: act, opt: targets} |> Poison.encode!
    %{user_id: user} = Mafia.Queries.player_for_slot(slot)
    MeetChannel.new_message("meet:#{channel}", "vote", user, message)
    {:noreply, state}
  end
  def handle_info({:message, slot, message}, %{game: %{id: id}} = state) do
    %{user_id: user} = Mafia.Queries.player_for_slot(slot)
    GameChannel.new_message(id, "sys", user, to_string(message))
    {:noreply, state}
  end
  def handle_info({:flip, slot, flip}, %{game: %{id: id}} = state) do
    %{user_id: user} = Mafia.Queries.player_for_slot(slot)
    teams = Enum.map flip[:teams], &to_string/1
    roles = Enum.map flip[:roles], fn {:',', mods, role} ->
      %{role: role, mods: Enum.map(mods, &to_string/1)}
    end
    message = Poison.encode! %{roles: roles, teams: teams}
    GameChannel.new_message(id, "flip", user, message)
    {:noreply, state}
  end
  def handle_info({:end_game, winners}, %{game: game} = state) do
    # %{user_id: user} = Mafia.Queries.player_for_slot(slot)
    # GameChannel.new_message(id, "sys", user, message)
    {1, _} = Mafia.Game
    |> where(id: ^game.id, status: "ongoing")
    |> Repo.update_all(set: [status: "finished"])

    GameChannel.new_message(game.id, "end", nil, Poison.encode! %{winners: winners})

    {:stop, :normal, state}
  end
  def handle_info(s, _) do
    raise "unknown message:" ++ s
  end


  # helpers

  defdelegate atom(string), to: String, as: :to_existing_atom
  defdelegate charlist(string), to: String, as: :to_charlist

  def game_db do
    {:ok, db} = :erlog.new
    Enum.reduce @game_db_files, db, fn (file, db) ->
      case :erlog.consult('prolog/' ++ file, db) do
        {:ok, db} -> db
        {:error, err} -> raise {:error, file, err}
      end
    end
  end

  def load_setup(db, setup) do
    db = Enum.reduce setup.teams, db, fn (t, db) ->
      fact = {:setup_team, t.player, charlist(t.team)}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    db = Enum.reduce setup.roles, db, fn (r, db) ->
      target = if r.type == "team", do: charlist(r.str), else: r.nr
      role = {:',', Enum.map(r.mods, &charlist/1), atom(r.role)}
      fact = {:setup_role, atom(r.type), target, role}
      {{:succeed, _}, db} = :erlog.prove({:asserta, fact}, db)
      db
    end

    phases = Enum.map setup.phases, &String.to_existing_atom/1
    {{:succeed, _}, db} = :erlog.prove({:asserta, {:setup_phases, phases}}, db)
    db
  end

  def load_slots(db, slots) do
    Enum.reduce slots, db, fn (slot, db) ->
      {{:succeed, _}, db} = :erlog.prove({:asserta, {:player, slot.id, slot.setup_player}}, db)
      db
    end
  end
end
