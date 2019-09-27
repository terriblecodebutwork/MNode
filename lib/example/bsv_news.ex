defmodule BsvNews do
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Wallet.PrivateKey
  alias Bex.CoinManager
  alias Bex.Wallet
  alias BexLib.Key
  require Logger

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

  def handle_cast({:hook_msg, %{id: id, txid: txid, data: data}}, state) do
    if Enum.any?(state.pool, fn x -> x.id == id end) do
      Logger.info("duplicate webhook, txid: #{txid}")
      {:noreply, state}
    else
      # new webhook msg
      pool = MapSet.put(state.pool, %{id: id, timestamp: timestamp()})

      case get_and_validate_utxo(txid) do
        false ->
          Logger.info("invalid utxo, txid: #{txid}")

        utxo ->
          Wallet.save_utxo(utxo)
          build_mnode(state.base_key.id, data, utxo)
      end

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

  @address "1Z2c8YiWRXGFj3zUWapfsEEJj1Qi482jZ"
  @script Key.address_to_pkscript(@address)
  @value Decimal.cast(10000)

  def get_and_validate_utxo(txid) do
    {:ok, rawtx} = SvApi.transaction(txid)
    {:ok, tx} = BexLib.Parser.parse_rawtx(rawtx)

    case Enum.find(tx.output, fn x -> x.raw_script == @script end) do
      nil ->
        false

      out ->
        v = Decimal.cast(out.value)

        case v |> Decimal.cmp(@value) do
          :lt ->
            false

          _ ->
            %{txid: txid, value: v, index: out.index, lock_script: @script}
        end
    end
  end
end
