defmodule Mafia.Prolog do
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

    
  def create(q) do
    body = Poison.encode! %{format: "json", destroy: false, ask: q}
    %{body: response} = HTTPoison.post! "localhost:5000/pengine/create", body, %{"Content-Type" => "application/json"}
    case Poison.decode!(response) do
      %{"id" => id, "event" => "create", "answer" => answer} ->
	{e, data} = handle_answer(answer)
	{e, id, data}
    end
  end
  
  def handle_answer(%{"event" => "success", "data" => data}), do: {:ok, data}
  def handle_answer(%{"event" => "error", "data" => data}), do: {:error, data}
  def handle_answer(%{"event" => "failure"}), do: {:fail, nil}
  
  def create!(q) do
    {:ok, id, a} = create(q)
    {id, a}
  end

  def ask(id, q) do
    %{body: response} = HTTPoison.get! "localhost:5000/pengine/send", [], params: %{format: "json", id: id, event: "ask(#{q}, [])"}
    %{"event" => event, "data" => data} = Poison.decode!(response)
    {event, data}
  end
  def ask!(id, q) do
    {"success", data} = ask(id, q)
    data
  end
end
