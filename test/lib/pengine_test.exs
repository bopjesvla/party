defmodule Mafia.PengineTest do
  use ExUnit.Case, async: true
  alias Mafia.Pengine

  test "prologize" do
    assert Pengine.prologize(5) == "5"
    assert Pengine.prologize(%{a: 5, b: ["'", 7]}) == ~S(m{'a':5,'b':['\'',7]})
  end
end
