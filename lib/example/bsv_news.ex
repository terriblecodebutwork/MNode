defmodule BsvNews do
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Wallet.PrivateKey
  alias Bex.CoinManager
  alias Bex.Wallet
  use GenServer
  @address "1Z2c8YiWRXGFj3zUWapfsEEJj1Qi482jZ"
  @root_node "BsvNews"

  def hook_msg(post) do
    GenServer.cast(__MODULE__, {:hook_msg, post})
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @clear_interval 60_000
  @live_time @clear_interval |> div(2)

  ## CALLBACKS

  def init(_) do
    base_key = find_key(@address)

    state = %{
      base_key: base_key,
      pool: MapSet.new()
    }

    :timer.send_interval(:clear_pool, @clear_interval)
    {:ok, state}
  end

  def handle_cast({:hook_msg, %{id: id, utxo: utxo, data: data}}, state) do
    if Enum.any?(state.pool, fn x -> x.id == id end) do
      {:noreply, state}
    else
      Wallet.save_utxo(utxo)
      pool = MapSet.put(state.pool, %{id: id, timestamp: timestamp()})
      build_mnode(state.base_key.id, data, utxo)
      {:noreply, %{state | pool: pool}}
    end
  end

  def handle_info(:clear_pool, state) do
    now = timestamp()
    pool = Enum.reject(state.pool, fn x -> now - x.timestamp > @live_time end)
    {:noreply, %{state | pool: pool}}
  end

  defp timestamp do
    :os.system_time(:seconds)
  end

  defp find_key(address) do
    from(p in PrivateKey, where: p.address == ^address)
    |> Repo.one!()
  end

  defp build_mnode(key, data, utxo) do
    case data.content do
      {:story, _title, _txid} ->
        contents = [
          Jason.encode!(%{
            txid: utxo.txid,
            mb_uid: data.uid,
            mb_username: data.user_name
          })
        ]

        CoinManager.create_mnode(key, @root_node, utxo.txid, contents)

      {:comment, parent, _data} ->
        contents = [
          Jason.encode!(%{
            txid: utxo.txid,
            mb_uid: data.uid,
            mb_username: data.user_name
          })
        ]

        CoinManager.create_mnode(key, parent, utxo.txid, contents)
    end
  end
end
