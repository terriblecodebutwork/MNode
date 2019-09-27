defmodule Bex.Txrepo do
  @moduledoc """
  #FIXME should use PSQL to persistant the txs.
  """
  use GenServer
  alias BexLib.Bitindex
  require Logger

  @broadcast true

  def start_link(_) do
    GenServer.start_link(
      __MODULE__,
      %{
        queue: :queue.new()
      },
      name: __MODULE__
    )
  end

  def add(txid, hex_tx) when is_binary(hex_tx) and is_binary(txid) do
    GenServer.cast(__MODULE__, {:add, txid, hex_tx})
  end

  def start_broadcast() do
    GenServer.cast(__MODULE__, :start_broadcast)
  end

  @interval 1000

  def init(state) do
    if @broadcast do
      :timer.send_after(@interval, :broadcast)
    end

    {:ok, state}
  end

  def handle_cast({:add, txid, hex_tx}, state = %{queue: q}) do
    {:noreply, %{state | queue: :queue.in({txid, hex_tx}, q)}}
  end

  def handle_cast(:start_broadcast, state) do
    :timer.send_after(@interval, :broadcast)
    {:noreply, state}
  end

  def handle_info(:broadcast, state = %{queue: q}) do
    case :queue.out(q) do
      {{:value, {txid, hex_tx}}, q} ->
        Logger.debug("broadcasting: " <> txid)
        {:ok, _} = do_broadcast(hex_tx)
        :timer.send_after(@interval, :broadcast)
        {:noreply, %{state | queue: q}}

      _ ->
        :timer.send_after(@interval, :broadcast)
        {:noreply, state}
    end
  end

  defp do_broadcast(tx) do
    Logger.info(inspect(Bitindex.broadcast_hex_tx(tx)))
  end
end
