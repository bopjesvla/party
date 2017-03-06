defmodule Mafia.AcceptanceCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use Wallaby.DSL

      alias Mafia.Repo
      import Ecto
      import Ecto.Changeset
      import Ecto.Query

      import Mafia.Router.Helpers
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Mafia.Repo)
    {:ok, _} = Application.ensure_all_started(:wallaby)
    Application.put_env(Mafia.Endpoint, :mafia, server: true, http: [port: 123])

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(Mafia.Repo, {:shared, self()})
    end

    metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(Mafia.Repo, self())
    {:ok, session} = Wallaby.start_session(metadata: metadata)
    {:ok, session: session}
  end
end
