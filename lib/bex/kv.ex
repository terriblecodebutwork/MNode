defmodule Bex.KV do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def save_tx(hex_tx) do
    key = {:tx, DateTime.utc_now()}
    put(key , hex_tx)
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def select(options) do
    GenServer.call(__MODULE__, {:select, options})
  end

  def put(key, value) do
    GenServer.call(__MODULE__, {:put, key, value})
  end

  ## Callbacks
  def init(_) do
    {:ok, %{}, {:continue, :start_cub}}
  end

  def handle_continue(:start_cub, _state) do
    {:ok, pid} = CubDB.start_link("cub")
    {:noreply, %{pid: pid}}
  end

  def handle_call({:get, key}, _from, s = %{pid: pid}) do
    {:reply, CubDB.get(pid, key), s}
  end

  def handle_call({:put, k, v}, _, s = %{pid: pid}) do
    {:reply, CubDB.put(pid, k, v), s}
  end

  def handle_call({:select, option}, _, s = %{pid: pid}) do
    {:reply, CubDB.select(pid, option), s}
  end
end
