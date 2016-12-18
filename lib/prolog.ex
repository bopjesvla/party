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
end
