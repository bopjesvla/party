defmodule Mafia.PrologTest do
  use ExUnit.Case, async: true
  alias Mafia.Prolog

  test "prologize" do
    assert Prolog.prologize(5) == "5"
    assert Prolog.prologize(%{a: 5, b: ["'", 7]}) == ~S(m{'a':5,'b':['\'',7]})
  end
end
