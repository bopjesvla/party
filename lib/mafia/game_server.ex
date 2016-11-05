defmodule Mafia.GameServer do
  use GenServer

  defstruct id: 1, creator: 1, users: [], players: []

  defp name(id) do
    {:via, :gproc, {:n, :l, {:game, id}}}
  end

  def start_link(id, user_id, opts) do
    GenServer.start_link __MODULE__, {id, user_id, opts}, name: name(id)
  end

  def init({id, user_id, opts}) do
    %GameServer{id: id, creator: user_id}
  end
end
