defmodule Bex.Broadcaster do
  @moduledoc """
  send rawtx into mempool via p2p network(aka. tcp connection with bsvnode)
  """
  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(
      __MODULE__,
      %{
        nodes: []
      },
      name: __MODULE__
    )
  end

  @doc """
  send raw tx (hex) to all nodes we known.
  """
  def send_all(tx) do
    GenServer.cast(__MODULE__, {:send_all, tx})
  end

  @doc """
  get a list of pending txs.
  """
  def list_nodes() do
    GenServer.call(__MODULE__, :list_nodes)
  end

  def init(state) do
    {:ok, state, {:continue, :get_nodes}}
  end

  def handle_continue(:get_nodes, state) do
    pids = reconnect()
    {:noreply, %{state | nodes: pids}}
  end

  # # reconnect all nodes every 30 seconds
  # def handle_info(:reconnect, state) do
  #   Logger.info "reconnect"
  #   pids = reconnect()
  #   {:noreply, %{ state | nodes: pids} }
  # end

  def handle_call(:list_nodes, _from, state = %{nodes: nodes}) do
    {:reply, nodes, state}
  end

  def handle_cast({:send_all, tx}, state = %{nodes: nodes}) do
    binary_tx = tx |> Binary.from_hex()

    for pid <- nodes do
      send(pid, {:tx, binary_tx})
    end

    # spawn_link(fn -> check_tx(tx) end)
    {:noreply, state}
  end

  ## helpers

  # get pids
  defp reconnect() do
    nodes = :sv_peer.get_addrs_ipv4_dns()

    for host <- nodes do
      :sv_peer.connect(host)
    end
  end

  def log_node() do
    ip = Enum.random(:sv_peer.get_addrs_ipv4_dns())
    p = :sv_peer.connect(ip, self())
    loop(p)
  end

  defp loop(p) do
    receive do
      {'version', _} ->
        send(p, :getheaders)

      {_, _} = d ->
        File.write("node.log", inspect(d), [:append])
        File.write("node.log", "\n", [:append])

      other ->
        Logger.warn("#{__MODULE__} got: " <> inspect(other))
    end

    loop(p)
  end
end
