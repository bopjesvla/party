defmodule Mafia.Pengine do
  use EEx.Engine
  
  def handle_expr(buffer, "=", expr) do
    quote do
      b = unquote(buffer)
      b <> Mafia.Pengine.prologize(unquote(expr))
    end
  end
  
  # end of EEx engine
  
  alias Mafia.{Repo, Channel, Message}
  
  def wrap_in_quotes(x), do: "'#{x}'"
  
  def prologize(str) when is_binary(str) do
    escaped = str |> String.replace("\\", ~S(\\)) |> String.replace(~S('), ~S(\')) |> wrap_in_quotes
  end
  def prologize(atom) when is_atom(atom), do: atom |> to_string |> prologize
  def prologize(x) when is_integer(x), do: to_string x
  def prologize(list) when is_list(list), do: "[#{list |> Enum.map(&prologize/1) |> Enum.join(",")}]"
  def prologize(map) when is_map(map) do
    parts = for {key, val} <- map, do: prologize(key) <> ":" <> prologize(val)
    "m{#{Enum.join parts, ","}}"
  end
    
  def create(q, args \\ []) do
    q = EEx.eval_string q, args, engine: __MODULE__
    
    body = Poison.encode! %{format: "json", destroy: false, ask: q}
    %{body: response} = HTTPoison.post! "localhost:5000/pengine/create", body, %{"Content-Type" => "application/json"}
    case Poison.decode!(response) do
      %{"id" => id, "event" => "create", "answer" => answer} ->
      	{e, data} = handle_answer(answer)
      	{e, id, data}
    end
  end
  
  def handle_answer(%{"event" => "success", "data" => [data]}), do: {:ok, data}
  def handle_answer(%{"event" => "error", "data" => data}), do: {:error, data}
  def handle_answer(%{"event" => "failure"}), do: {:fail, nil}
  
  def create!(q, args \\ []) do
    {:ok, id, a} = create(q, args)
    {id, a}
  end

  def ask(game, q, args \\ []) do
    q = EEx.eval_string q, args, engine: __MODULE__
    
    %{body: response} = HTTPoison.get! "localhost:5000/pengine/send", [], params: %{format: "json", id: game.pengine, event: "ask((#{q}, flush(FlushMessages)), [])"}

    response = Poison.decode!(response) |> handle_answer

    case response do
      {:ok, %{"FlushMessages" => flush_messages}} ->
        Enum.each flush_messages, &(handle_message(game, &1))
        response
      _ ->
        response
    end
  end
  
  def ask_after(delay, game, q) do
    spawn fn ->
      Process.sleep(delay * 1000)
      ask!(game, q)
    end
  end

  def handle_message(game, %{"functor" => f, "args" => a}), do: handle_message(game, {f, a})
  def handle_message(game, {"create_channel", [channel]}), do: Repo.insert!(%Channel{game_id: game.id, name: channel, type: "m"})
  def handle_message(game, {"join", [user, channel]}), do: Repo.insert!(%Message{channel: Repo.get_by(Channel, name: channel, game_id: game.id, type: "m"), user_id: user, type: "j"})
  def handle_message(game, {"check_after", [delay]}) do
    # __MODULE__ for test mocking
    __MODULE__.ask_after(delay, game, "true")
  end
  def handle_message(game, {"leave", [who, channel]}), do: Mafia.Endpoint.broadcast! "meet:#{channel}", "leave", %{who: who}
  
  def ask!(id, q, args \\ []) do
    {:ok, data} = ask(id, q, args)
    data
  end
end
