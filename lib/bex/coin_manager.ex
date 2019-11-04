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
  alias BexLib.Txmaker
  import Ecto.Query
  require Logger

  use GenServer

  @coin_sat Decimal.cast(90_500)
  @permission_num 1

  ## FIXME don't do anything when inputs is empty
  def sweep(key, target) do
    inputs = key |> Repo.preload(:utxos) |> Map.get(:utxos)
    do_sweep(inputs, target)
  end

  def sweep_utxo(utxo_id, target) do
    inputs = [Repo.get(Utxo, utxo_id)]
    do_sweep(inputs, target)
  end

  defp do_sweep(inputs, target) do
    inputs_value = Utxo.sum_of_value(inputs)
    fee = Txmaker.estimate_tx_fee(length(inputs), 1, true, false)
    outputs = [Utxo.address_utxo(target, Decimal.sub(inputs_value, fee))]
    {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, @coin_sat)
    Txrepo.add(txid, hex_tx)
    {:ok, txid, hex_tx}
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

  def make(args) do
    GenServer.call(__MODULE__, {:make, args})
  end

  def recast(pkid) do
    GenServer.call(__MODULE__, {:recast, pkid})
  end

  def recast(pkid, coin_sat) do
    GenServer.call(__MODULE__, {:recast, pkid, coin_sat})
  end

  def transfer(pkid, args) do
    GenServer.call(__MODULE__, {:transfer, pkid, args})
  end

  @doc """
  split all utxos which larger than coin_sat into coins.
  """
  def mint(pkid) do
    GenServer.call(__MODULE__, {:mint, pkid})
  end

  def mint(pkid, coin_sat) do
    GenServer.call(__MODULE__, {:mint, pkid, coin_sat})
  end

  def mint(pkid, coin_sat, opt) do
    GenServer.call(__MODULE__, {:mint, pkid, coin_sat, opt})
  end

  def exists?(dir) do
    from(p in PrivateKey, where: p.dir == ^dir) |> Repo.exists?()
  end

  @doc """
  iname: the name of new mnode
  pname: the name of parent mnode
  """
  def create_mnode(pkid, pname, iname, content, opts \\ [])

  def create_mnode(pkid, false, iname, content, opts) when is_binary(iname) do
    if exists?(iname) do
      {:error, "#{iname} already exists"}
    else
      Logger.info("creating root mnode: #{iname}")

      r =
        Repo.transaction(fn ->
          do_create_root_mnode(pkid, iname, content, opts[:coin_sat] || @coin_sat, opts)
        end)
        |> extract_transaction()

      Logger.info(inspect(r))
      r
    end
  end

  def create_mnode(pkid, pname, iname, content, opts)
      when is_binary(iname) and is_binary(pname) do
    if exists?(iname) do
      {:error, "#{iname} already exists"}
    else
      Logger.info("creating child mnode: #{pname} >> #{iname}")

      r =
        Repo.transaction(fn ->
          do_create_sub_mnode(pkid, pname, iname, content, opts[:coin_sat] || @coin_sat, opts)
        end)
        |> extract_transaction()

      Logger.info(inspect(r))
      r
    end
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

  defp query_to_get_coin(pkid, n, coin_sat) do
    from(u in Utxo,
      where: u.value == ^coin_sat and u.private_key_id == ^pkid,
      limit: ^n,
      lock: "FOR UPDATE SKIP LOCKED",
      order_by: [asc: :id]
    )
    |> Repo.all()
  end

  def handle_call(:get_coin_sat, _from, state) do
    {:reply, state.coin_sat, state}
  end

  def handle_call({:recast, pkid}, _from, state) do
    {:reply, do_recast(pkid, state.coin_sat), state}
  end

  def handle_call({:recast, pkid, coin_sat}, _from, state) do
    {:reply, do_recast(pkid, coin_sat), state}
  end

  def handle_call({:recast, pkid, coin_sat, opt}, _from, state) do
    {:reply, do_recast(pkid, coin_sat, opt), state}
  end

  def handle_call({:mint, pkid}, _from, state) do
    {:reply, do_mint(pkid, state.coin_sat), state}
  end

  def handle_call({:mint, pkid, coin_sat}, _from, state) do
    {:reply, do_mint(pkid, coin_sat), state}
  end

  def handle_call({:mint, pkid, coin_sat, opt}, _from, state) do
    {:reply, do_mint(pkid, coin_sat, opt), state}
  end

  def handle_call({:transfer, pkid, args}, _from, state) do
    {:reply, do_transfer(pkid, args), state}
  end

  def handle_call({:create_root_mnode, pkid, iname, content, opts}, _from, state) do
    {:reply, do_create_root_mnode(pkid, iname, content, opts[:coin_sat] || state.coin_sat, opts),
     state}
  end

  def handle_call({:create_sub_mnode, pkid, pname, iname, content, opts}, _from, state) do
    {:reply,
     do_create_sub_mnode(pkid, pname, iname, content, opts[:coin_sat] || state.coin_sat, opts),
     state}
  end

  def handle_call({:make, args}, state) do
    {:reply, do_make(args), state}
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
    utxos = query_to_get_coin(pkid, n, coin_sat)

    if length(utxos) == n do
      {:ok, utxos}
    else
      do_mint(pkid, coin_sat)
      try_to_recast_dusts(pkid, n, coin_sat)
    end
  end

  defp do_recast(pkid, coin_sat, opt \\ []) do
    p = Repo.get!(PrivateKey, pkid)

    dusts =
      from(u in Utxo,
        where: u.private_key_id == ^p.id and u.type == "dust",
        limit: 100
      )
      |> Repo.all()

    inputs = dusts
    inputs_value = Utxo.sum_of_value(inputs)
    coin_num = Decimal.div_int(inputs_value, coin_sat) |> Decimal.to_integer()

    if coin_num == 0 do
      {:error, "not enough dusts"}
    else
      change_pkid = p.id

      p =
        case opt do
          [] ->
            p

          [to: key2] ->
            key2
        end

      change_script = Key.private_key_to_p2pkh_script(p.bn)
      coin_utxo = %Utxo{value: coin_sat, private_key_id: p.id, lock_script: change_script}
      outputs = List.duplicate(coin_utxo, coin_num)

      case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
        {:error, msg} ->
          {:error, msg}

        {:ok, inputs, outputs} ->
          {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, coin_sat)
          Txrepo.add(txid, hex_tx)
          {:ok, txid, hex_tx}
      end
    end
  end

  defp do_mint(pkid, coin_sat, opt \\ []) do
    p = Repo.get!(PrivateKey, pkid) |> Repo.preload(:utxos)

    case Utxo.mint_all(p, coin_sat, opt) do
      {:ok, txid, hex_tx} ->
        Txrepo.add(txid, hex_tx)
        {:ok, txid, hex_tx}

      {:error, msg} ->
        {:error, msg}
    end
  end

  @fee_rate 1

  defp do_transfer(pkid, %{to: addr, amount: v}) do
    p = Repo.get!(PrivateKey, pkid)
    # how many inputs needed
    n = floor(div(v, 90500)) + 1
    # size(vin) + n*148 + others
    size = 24 + n * 148 + 500
    fee = size * @fee_rate

    # real need inputs
    m = floor(div(fee + v, 90500)) + 1

    case do_get_coins(pkid, m, Decimal.cast(90500)) do
      {:ok, utxos} ->
        inputs = utxos

        outputs = [
          Utxo.address_utxo(addr, Decimal.cast(v))
        ]

        {change_script, change_pkid} = Utxo.change_to_address(p)

        case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
          {:error, msg} ->
            {:error, msg}

          {:ok, inputs, outputs} ->
            {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, Decimal.cast(90500))
            Txrepo.add(txid, hex_tx)
            {:ok, txid, hex_tx}
        end

      {:error, msg} ->
        {:error, msg}
    end
  end

  @doc """
  use a utxo that equal to coin_sat
  """
  def send_opreturn(pkid, contents, coin_sat \\ @coin_sat, opts \\ []) do
    Repo.transaction(fn ->
      n =
        case opts[:inputs] do
          nil -> 1
          x when is_integer(x) -> x
        end

      p = Repo.get!(PrivateKey, pkid)
      {:ok, inputs} = do_get_coins(pkid, n, coin_sat)
      outputs = [Utxo.return_utxo(contents)]

      {change_script, change_pkid} = Utxo.change_to_address(p, opts)

      case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
        {:error, msg} ->
          {:error, msg}

        {:ok, inputs, outputs} ->
          {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, coin_sat)
          Txrepo.add(txid, hex_tx)
          {:ok, txid, hex_tx}
      end
    end)
    |> extract_transaction()
  end

  defp extract_transaction({:ok, any}), do: any
  defp extract_transaction(other), do: other

  def test_concurrent(f) do
    spawn(f)
    spawn(f)
  end

  @doc """
  send a single utxo to a address

  1 in 1 out tx's size is 192 B (compressed sign 191B)
  """
  def send_to_address(pkid, address, coin_sat) do
    {:ok, inputs} = do_get_coins(pkid, 1, coin_sat)
    outputs = [Utxo.address_utxo(address, Decimal.sub(hd(inputs).value, 193))]
    {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, coin_sat)
    Txrepo.add(txid, hex_tx)
    {:ok, txid, hex_tx}
  end

  defp do_create_root_mnode(pkid, iname, content, coin_sat, opts) do
    p = Repo.get!(PrivateKey, pkid)
    case do_get_coins(pkid, 1, coin_sat) do
      {:ok, inputs} ->
        {:ok, c_key} = Wallet.derive_and_insert_key(p, p, iname)

        c_permission_utxo = Utxo.c_permission_utxo(c_key)

        meta = Utxo.meta_utxo(c_key.address, content)
        outputs = [meta | List.duplicate(c_permission_utxo, @permission_num)]

        {change_script, change_pkid} = Utxo.change_to_address(p, opts)

        case Utxo.handle_change(inputs, outputs, change_script, change_pkid) do
          {:error, msg} ->
            {:error, msg}

          {:ok, inputs, outputs} ->
            {:ok, txid, hex_tx} = Utxo.make_tx(inputs, outputs, coin_sat)
            Wallet.update_private_key(c_key, %{dir_txid: txid})
            Txrepo.add(txid, hex_tx)
            {:ok, txid, hex_tx}
        end
      {:error, msg} ->
        {:error, msg}
    end
  end

  defp do_create_sub_mnode(pkid, pname, iname, content, coin_sat, opts) do
    p = Repo.get!(PrivateKey, pkid)

    case Wallet.find_key_with_dir(p, pname) do
      {:ok, p_key} ->
        # parent key exists
        case Utxo.create_sub_dir(p_key, iname, content, coin_sat, opts) do
          {:ok, txid, hex_tx} ->
            Txrepo.add(txid, hex_tx)
            {:ok, txid, hex_tx}

          {:error, msg} ->
            {:error, msg}
        end

      {:error, _msg} ->
        {:error, "Dir '#{pname}' isn't exist."}
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

  def do_make(args) do
    # TODO
    args
  end
end
