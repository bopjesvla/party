defmodule Mafia.PrologTest do
  use ExUnit.Case, async: true
  
  test "mafia.pl tests" do
    assert {:ok, erlog} = :erlog.new
    assert {:ok, erlog} = :erlog.consult('mafia.pl', erlog)
    assert {:ok, erlog} = :erlog.consult('utils.pl', erlog)
    assert {:ok, erlog} = :erlog.consult('mafia.plt', erlog)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, erlog)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
end
