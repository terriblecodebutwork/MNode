defmodule Bex.CoinManager do
  use GenServer

  @coin_sat Decimal.cast(10_000)

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{coin_sat: @coin_sat}, name: __MODULE__)
  end

  def set_coin_sat(n) do
    GenServer.cast(__MODULE__, {:set_coin_sat, n})
  end

  def get_coin_sat() do
    GenServer.call(__MODULE__, :get_coin_sat)
  end

  def init(state) do
    {:ok, state}
  end

  def handle_call(:get_coin_sat, _from, state) do
    {:reply, state.coin_sat, state}
  end

  def handle_cast({:set_coin_sat, n}, state) do
    {:noreply, %{state| coin_sat: n}}
  end
end