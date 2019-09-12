defmodule BsvNews do
  use GenServer

  def new_post(post) do
    GenServer.cast(__MODULE__, {:new_post, post})
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @clear_interval 60_000
  @live_time @clear_interval |> div(2)

  ## CALLBACKS

  def init(_) do
    state = %{
      pool: MapSet.new()
    }
    :timer.send_interval(:clear_pool, @clear_interval)
    {:ok, state}
  end

  def handle_cast({:new_post, %{id: id, user_id: uid, utxo: utxo} }, state) do
    if Enum.any?(state.pool, fn x -> x.id == id end) do
      {:noreply, state}
    else
      pool = MapSet.put(state.pool, %{id: id, timestamp: timestamp()})
      create_mnode(uid, utxo.txid)
      {:noreply, %{ state | pool: pool}}
    end
  end

  def handle_info(:clear_pool, state) do
    now = timestamp()
    pool = Enum.reject(state.pool, fn x -> now - x.timestamp > @live_time end)
    {:noreply, %{ state | pool: pool} }
  end

  defp create_mnode(uid, txid) do
    IO.puts "TODO save the #{txid} under mb_user_#{uid} directory"
  end

  defp timestamp do
    :os.system_time(:seconds)
  end
end