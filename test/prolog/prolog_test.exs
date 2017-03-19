defmodule Mafia.PrologTest do
  use ExUnit.Case, async: true

  @moduletag :prolog

  setup do
    db = Mafia.GameServer.game_db
    {:ok, db: db}
  end

  test "mafia.pl tests", %{db: db} do
    db = db
    |> Mafia.GameServer.consult("simple_setup.plt")
    |> Mafia.GameServer.consult("mafia.plt")
    
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end

  test "resolve.pl tests", %{db: db} do
    db = Mafia.GameServer.consult(db, "resolve.plt")
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end

  test "actions.pl tests", %{db: db} do
    db = db
    |> Mafia.GameServer.consult("simple_setup.plt")
    |> Mafia.GameServer.consult("actions.plt")
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end

  test "roles.pl tests", %{db: db} do
    db = db
    |> Mafia.GameServer.consult("simple_setup.plt")
    |> Mafia.GameServer.consult("roles.plt")
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end

  test "utils.pl tests", %{db: db} do
    db = db |> Mafia.GameServer.consult("utils.plt")
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
end
