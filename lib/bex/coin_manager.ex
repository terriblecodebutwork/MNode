defmodule Bex.CoinManager do
  @moduledoc """
  Give coins to other processes.
  It is a single process, so no need lock.(Maybe should
  be fixed)
  #FIXME
  now everything running in this process, query coins, recast, make transaction, all because I don't know
  how to use PSQL's lock.
  """
  alias Bex.Repo
  alias Bex.Wallet
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.PrivateKey
  alias BexLib.Key
  alias Bex.Txrepo
  import Ecto.Query
  require Logger

  use GenServer

  @coin_sat Decimal.cast(10_000)
  @permission_num 1

  def sync_utxo(pkid) do
    p = Repo.get!(PrivateKey, pkid)
    Wallet.sync_utxos_of_private_key(p)
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{coin_sat: @coin_sat}, name: __MODULE__)
  end

  # FIXME should support one pkid one coin_sat
  def set_coin_sat(v) do
    v = Decimal.cast(v)
    GenServer.cast(__MODULE__, {:set_coin_sat, v})
  end

  def get_coin_sat() do
    GenServer.call(__MODULE__, :get_coin_sat)
  end

  def recast(pkid) do
    GenServer.call(__MODULE__, {:recast, pkid})
  end

  @doc """
  split all utxos which larger than coin_sat into coins.
  """
  def mint(pkid) do
    GenServer.call(__MODULE__, {:mint, pkid})
  end

  @doc """
  Get n coins of one private key.
  #FIXME
  should not use this, it will cause race condition.
  """
  def get_coins(pkid, n) do
    GenServer.call(__MODULE__, {:get_coins, pkid, n})
  end

  @doc """
  iname: the name of new mnode
  pname: the name of parent mnode
  """
  def create_mnode(pkid, false, iname, content) when is_binary(iname) do
    Logger.info("creating root mnode: #{iname}")
    Logger.info inspect(GenServer.call(__MODULE__, {:create_root_mnode, pkid, iname, content}))
  end

  def create_mnode(pkid, pname, iname, content) when is_binary(iname) and is_binary(pname) do
    Logger.info("creating child mnode: #{pname} >> #{iname}")
    Logger.info inspect(GenServer.call(__MODULE__, {:create_sub_mnode, pkid, pname, iname, content}))
  end

  def init(state) do
    coin_sat =
      case from(p in PrivateKey,
             where: is_nil(p.base_key_id),
             limit: 1
           )
           |> Repo.one() do
        nil ->
          @coin_sat

        p ->
          case from(u in Utxo,
                 where: u.private_key_id == ^p.id and u.type == "coin",
                 limit: 1
               )
               |> Repo.one() do
            nil -> @coin_sat
            u -> u.value
          end
      end

    {:ok, %{state | coin_sat: coin_sat}}
  end

  defp query_to_get_coin(pkid, n) do
    from(u in Utxo,
      where: u.type == "coin" and u.private_key_id == ^pkid,
      limit: ^n,
      lock: "FOR UPDATE SKIP LOCKED",
      order_by: [asc: :id]
    )
    |> Repo.all()
  end

  def handle_call({:get_coins, pkid, n}, _from, state) do
    {:reply, do_get_coins(pkid, n, state.coin_sat), state}
  end

  def handle_call(:get_coin_sat, _from, state) do
    {:reply, state.coin_sat, state}
  end

  def handle_call({:recast, pkid}, _from, state) do
    {:reply, do_recast(pkid, state.coin_sat), state}
  end

  def handle_call({:mint, pkid}, _from, state) do
    {:reply, do_mint(pkid, state.coin_sat), state}
  end

  def handle_call({:create_root_mnode, pkid, iname, content}, _from, state) do
    {:reply, do_create_root_mnode(pkid, iname, content, state.coin_sat), state}
  end

  def handle_call({:create_sub_mnode, pkid, pname, iname, content}, _from, state) do
    {:reply, do_create_sub_mnode(pkid, pname, iname, content, state.coin_sat), state}
  end

  def handle_cast({:set_coin_sat, v}, state) do
    do_update_utxos_type(v)
    {:noreply, %{state | coin_sat: v}}
  end

  defp try_to_recast_dusts(pkid, n, coin_sat) do
    case do_recast(pkid, coin_sat) do
      {:error, msg} ->
        Logger.error(msg)
        try_to_get_coins_again(pkid, n)

      {:ok, txid, hex_tx} ->
        Txrepo.add(txid, hex_tx)
        try_to_get_coins_again(pkid, n)
    end
  end

  defp try_to_get_coins_again(pkid, n) do
    utxos = query_to_get_coin(pkid, n)

    if length(utxos) == n do
      {:ok, utxos}
    else
      Logger.error("insufficient balance")
      {:error, "insufficient balance"}
    end
  end

  defp do_get_coins(pkid, n, coin_sat) do
    utxos = query_to_get_coin(pkid, n)

    if length(utxos) == n do
      {:ok, utxos}
    else
      do_mint(pkid, coin_sat)
      try_to_recast_dusts(pkid, n, coin_sat)
    end
  end

  defp do_recast(pkid, coin_sat) do
    p = Repo.get!(PrivateKey, pkid)

    dusts =
      from(u in Utxo,
        where: u.private_key_id == ^p.id and u.type == "dust"
      )
      |> Repo.all()

    inputs = dusts
    inputs_value = Utxo.sum_of_value(inputs)
    coin_num = Decimal.div_int(inputs_value, coin_sat) |> Decimal.to_integer()

    if coin_num == 0 do
      {:error, "not enough dusts"}
    else
      change_script = Key.private_key_to_p2pkh_script(p.bn)
      change_pkid = p.id
      coin_utxo = %Utxo{value: coin_sat, private_key_id: p.id, lock_script: change_script}
      outputs = List.duplicate(coin_utxo, coin_num)

      case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
        {:error, msg} ->
          {:error, msg}

        {:ok, inputs, outputs} ->
          Utxo.make_tx(inputs, outputs, coin_sat)
      end
    end
  end

  defp do_mint(pkid, coin_sat) do
    p = Repo.get!(PrivateKey, pkid) |> Repo.preload(:utxos)

    case Utxo.mint_all(p, coin_sat) do
      {:ok, txid, hex_tx} ->
        Txrepo.add(txid, hex_tx)
        {:ok, txid, hex_tx}

      {:error, msg} ->
        {:error, msg}
    end
  end

  defp do_create_root_mnode(pkid, iname, content, coin_sat) do
    p = Repo.get!(PrivateKey, pkid)
    {:ok, inputs} = do_get_coins(pkid, 1, coin_sat)
    {:ok, c_key} = Wallet.derive_and_insert_key(p, p, iname)

    c_permission_utxo = Utxo.c_permission_utxo(c_key)

    meta = Utxo.meta_utxo(c_key.address, content)
    outputs = [meta | List.duplicate(c_permission_utxo, @permission_num)]
    # send change to base key
    change_script = p.lock_script
    change_pkid = p.id

    case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
      {:error, msg} ->
        {:error, msg}

      {:ok, inputs, outputs} ->
        {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, coin_sat)
        Wallet.update_private_key(c_key, %{dir_txid: txid})
        Txrepo.add(txid, hex_tx)
        {:ok, txid, hex_tx}
    end
  end

  defp do_create_sub_mnode(pkid, pname, iname, content, coin_sat) do
    p = Repo.get!(PrivateKey, pkid)

    case Wallet.find_key_with_dir(p, pname) do
      {:ok, p_key} ->
        # parent key exists
        case Utxo.create_sub_dir(p_key, iname, content, coin_sat) do
          {:ok, txid, hex_tx} ->
            Txrepo.add(txid, hex_tx)
            {:ok, txid, hex_tx}

          {:error, msg} ->
            {:error, msg}
        end

      {:error, msg} ->
        {:error, msg}
    end
  end

  defp do_update_utxos_type(coin_sat) do
    from(u in Utxo, where: u.type == "coin" or u.type == "dust" or u.type == "gold")
    |> Repo.all()
    |> Enum.map(fn u ->
      Utxo.changeset(u, %{type: utxo_type(u.value, coin_sat)})
      |> Repo.update!()
    end)
  end

  defp utxo_type(v, coin_v) do
    case Decimal.cmp(v, coin_v) do
      :eq -> :coin
      :lt -> :dust
      :gt -> :gold
    end
  end
end
