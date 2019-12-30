defmodule Bex.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    children =
      [
        Bex.Txrepo,
        # BsvNews,
        Bex.KV,
        # Start the endpoint when the application starts
        BexWeb.Endpoint
        # Starts a worker by calling: Bex.Worker.start_link(arg)
        # {Bex.Worker, arg},
      ] ++
        if(System.get_env("BEX_BROADCASTER"), do: [Bex.Broadcaster], else: []) ++
        if(System.get_env("BEX_MERKLESAVER"), do: [Bex.Store.MerkleSaver], else: []) ++
        if(Application.get_env(:bex, :no_database, false), do: [], else: [Bex.Repo, Bex.CoinManager]) ++
        if(Application.get_env(:bex, :no_quickapi, false), do: [], else: [Bex.QuickApi])

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Bex.Supervisor]
    r = Supervisor.start_link(children, opts)
    bootstrap()
    r
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BexWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  def bootstrap() do
    if System.get_env("BexChat") do
      spawn(fn ->
        Bex.ChatEngine.init_chatnode()
      end)
    end
  end
end
