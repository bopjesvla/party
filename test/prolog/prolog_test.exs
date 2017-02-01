defmodule Mafia.PrologTest do
  use ExUnit.Case, async: true
  
  setup do
    db = Mafia.GameServer.game_db
    {:ok, db: db}
  end
  
  test "mafia.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('prolog/simple_setup.plt', db)
    assert {:ok, db} = :erlog.consult('prolog/mafia.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
  
  test "resolve.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('prolog/resolve.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
  
  test "roles.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('prolog/simple_setup.plt', db)
    assert {:ok, db} = :erlog.consult('prolog/roles.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
end
