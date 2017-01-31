defmodule Mafia.PrologTest do
  use ExUnit.Case, async: true
  
  setup do
    assert {:ok, db} = :erlog.new
    assert {:ok, db} = :erlog.consult('mafia.pl', db)
    assert {:ok, db} = :erlog.consult('roles.pl', db)
    assert {:ok, db} = :erlog.consult('actions.pl', db)
    assert {:ok, db} = :erlog.consult('resolve.pl', db)
    assert {:ok, db} = :erlog.consult('utils.pl', db)
    {:ok, db: db}
  end
  
  test "mafia.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('simple_setup.plt', db)
    assert {:ok, db} = :erlog.consult('mafia.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
  
  test "resolve.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('resolve.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
  
  test "roles.pl tests", %{db: db} do
    assert {:ok, db} = :erlog.consult('simple_setup.plt', db)
    assert {:ok, db} = :erlog.consult('roles.plt', db)
    assert {result, _} = :erlog.prove({:run_tests, {:errors}}, db)
    assert {:succeed, [errors: errors]} = result
    assert [] = errors
  end
end
