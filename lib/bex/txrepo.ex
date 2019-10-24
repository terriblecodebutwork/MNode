defmodule Bex.Txrepo do
  @moduledoc """
  #FIXME should use PSQL to persistant the txs.
  """
  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(
      __MODULE__,
      %{
        queue: :queue.new(),
        status: :on
      },
      name: __MODULE__
    )
  end

  # def add(txid, hex_tx) when is_binary(hex_tx) and is_binary(txid) do
  #   GenServer.cast(__MODULE__, {:add, txid, hex_tx})
  # end

  # FIXME
  def add(txid, tx) do
    spawn_link(fn ->
      try_broadcast(txid, tx)
    end)
  end

  def pending(info) do
    GenServer.cast(__MODULE__, {:pending, info})
  end

  defp try_broadcast(txid, tx) do
    case SvApi.broadcast(tx) do
      {:ok, _} -> :ok
      {:error, msg} ->
        pending({txid, tx, msg})
    end
  end

  def turn_on() do
    GenServer.cast(__MODULE__, :turn_on)
  end

  def turn_off() do
    GenServer.cast(__MODULE__, :turn_off)
  end

  @doc """
  get a list of pending txs.
  """
  def list() do
    GenServer.call(__MODULE__, :list)
  end

  def init(state) do
    if state.status == :on do
      send(self(), :broadcast)
    end

    {:ok, state}
  end

  def handle_call(:list, _from, state = %{queue: q}) do
    {:reply, :queue.to_list(q), state}
  end

  def handle_cast({:pending, info}, state = %{queue: q}) do
    {:noreply, %{state | queue: :queue.in(info, q)}}
  end

  def handle_cast(:turn_on, state) do
    send(self(), :broadcast)
    {:noreply, %{state | status: :on}}
  end

  def handle_cast(:turn_off, state) do
    {:noreply, %{state | status: :off}}
  end

  def handle_info(:broadcast, state = %{queue: q, status: :on}) do
    case :queue.out(q) do
      {{:value, {txid, hex_tx, _msg}}, q1} ->
        add(txid, hex_tx)
        :timer.send_after(60_000, :broadcast)
        {:noreply, %{state | queue: q1}}

      _ ->
        :timer.send_after(60_000, :broadcast)
        {:noreply, state}
    end
  end

  def handle_info(:broadcast, state) do
    Logger.info("TxRepo is off.")
    {:noreply, state}
  end

  def handle_info(other, state) do
    Logger.error(inspect(other))
    {:noreply, state}
  end

  # defp do_broadcast(tx) do
  #   case SvApi.broadcast(tx) do
  #     {:error, msg} ->
  #       Logger.error(tx <> "\n" <> msg)
  #     _ ->
  #       nil
  #   end
  # end

  # defp do_broadcast(tx) do
  #   # FIXME there is no way to know is tx been accepted
  #   # maybe try to get the tx from SvApi?
  #   Bex.Broadcaster.send_all(tx)

  #   SvApi.broadcast(tx)
  #   |> inspect()
  #   |> Logger.debug()
  # end
end
