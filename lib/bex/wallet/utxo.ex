defmodule Bex.Wallet.Utxo do
  @moduledoc """
  only keep the spendable utxo.

  save the unbroadcasted tx(TODO), delete inputs utxos.
  """

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  alias Bex.Wallet.PrivateKey
  alias Bex.Wallet
  alias Bex.UtxoType
  alias BexLib.Txmaker
  alias Bex.Repo
  alias BexLib.Key
  alias BexLib.Script
  alias __MODULE__
  alias Bex.CoinManager
  alias Bex.ChatEngine
  require Logger
  # alias Bex.Wallet.Mission

  @permission_sat Decimal.cast(546)
  @permission_num 1

  schema "utxos" do
    field :index, :integer
    field :lock_script, :binary
    field :txid, :string
    field :value, :decimal
    field :block_height, :integer
    field :type, UtxoType
    belongs_to :private_key, PrivateKey
    # belongs_to :consumer, Mission, foreign_key: :consumer_id
    # belongs_to :producer, Mission, foreign_key: :producer_id
  end

  @doc false
  def changeset(utxo, attrs) do
    utxo
    |> cast(attrs, [:value, :lock_script, :txid, :index, :type])
    |> cast_assoc(:private_key)
    |> validate_required([:value, :lock_script, :type])
  end

  def meta_utxo(addr, content, p_txid \\ "NULL") do
    content = content || []
    Logger.debug("meta_utxo: #{inspect({addr, content, p_txid})}")
    lock_script = Script.metanet(addr, content, p_txid)

    %Utxo{
      value: Decimal.cast(0),
      lock_script: lock_script,
      type: :data
    }
  end

  def return_utxo(contents) do
    %Utxo{
      value: Decimal.cast(0),
      lock_script: Script.return(contents),
      type: :data
    }
  end

  @doc """
  handle the :change_to option, and return the {lockscript, pkid}
  """
  def change_to_address(p, opts) do
    case opts[:change_to] do
      nil ->
        # send change to base key
        {p.lock_script, p.id}

      addr when is_binary(addr) ->
        {Key.address_to_pkscript(addr), nil}
    end
  end

  def address_utxo(address, value) do
    %Utxo{
      value: Decimal.cast(value),
      lock_script: Key.address_to_pkscript(address),
      type: :dust
    }
  end

  def set_utxo_type(u = %{type: type}, _coin_sat) when not is_nil(type) do
    u
  end

  @zero Decimal.cast(0)
  def set_utxo_type(utxo = %{value: @zero}, _) do
    Map.put(utxo, :type, :data)
  end

  def set_utxo_type(utxo = %{value: @permission_sat}, _) do
    Map.put(utxo, :type, :permission)
  end

  def set_utxo_type(utxo = %{value: v}, coin_sat) do
    type =
      case Decimal.cmp(v, coin_sat) do
        :lt -> :dust
        :eq -> :coin
        :gt -> :gold
      end

    Map.put(utxo, :type, type)
  end

  @doc """
  Split a gold into many coins. Build tx and broadcast.
  Return {:ok, any} or {:error, msg}
  """
  def mint(u = %__MODULE__{lock_script: s, value: v, private_key_id: pkid}) do
    coin_sat = CoinManager.get_coin_sat()
    inputs = [u]
    coin_num = Decimal.div_int(v, coin_sat) |> Decimal.to_integer()
    coin_utxo = %__MODULE__{value: coin_sat, private_key_id: pkid, lock_script: s}
    outputs = List.duplicate(coin_utxo, coin_num)
    change_script = s
    change_pkid = pkid

    case handle_change(inputs, outputs, change_script, change_pkid) do
      {:error, msg} ->
        {:error, msg}

      {:ok, inputs, outputs} ->
        make_tx(inputs, outputs, coin_sat)
    end
  end

  def mint_all(%PrivateKey{utxos: utxos, lock_script: s, id: pkid}, coin_sat, opt \\ []) do
    inputs = Enum.filter(utxos, fn u -> Decimal.cmp(u.value, coin_sat) == :gt end)

    if inputs !== [] do
      v = sum_of_value(inputs)
      coin_num = Decimal.div_int(v, coin_sat) |> Decimal.to_integer()

      coin_num =
        if coin_num > 1000 do
          1000
        else
          coin_num
        end

      outputs =
        case opt do
          [to: key2] ->
            coin_utxo = %__MODULE__{
              value: coin_sat,
              private_key_id: key2.id,
              lock_script: key2.lock_script
            }

            List.duplicate(coin_utxo, coin_num)

          [] ->
            coin_utxo = %__MODULE__{value: coin_sat, private_key_id: pkid, lock_script: s}
            List.duplicate(coin_utxo, coin_num)
        end

      change_script = s
      change_pkid = pkid

      case handle_change(inputs, outputs, change_script, change_pkid) do
        {:error, msg} ->
          {:error, msg}

        {:ok, inputs, outputs} ->
          make_tx(inputs, outputs, coin_sat)
      end
    else
      {:error, "nothing to mint"}
    end
  end

  # FIXME as one privateKey can only has
  # one unique lock_script
  # So we can drop one param,
  # search the db with change_script
  # or serche that with change_pkid
  def handle_change(inputs, outputs, change_script, change_pkid) do
    case get_change_amount(inputs, outputs) do
      {:error, msg} ->
        {:error, msg}

      {change, outputs} ->
        outputs = add_change(outputs, change, change_script, change_pkid)
        {:ok, inputs, outputs}
    end
  end

  def c_permission_utxo(c_key) do
    %__MODULE__{
      value: @permission_sat,
      private_key_id: c_key.id,
      type: :permission,
      lock_script: c_key.lock_script
    }
  end

  # dir is full dir
  # c_ child
  # s_ self
  def create_sub_dir(s_key, c_dir, content, coin_sat, opts) do
    base_key = Repo.preload(s_key, :base_key).base_key
    s_permission = Wallet.get_a_permission(s_key)

    funds =
      case opts[:fund] do
        {id, n} ->
          Wallet.get_coins(Wallet.get_private_key!(id), coin_sat, n)

        nil ->
          Wallet.get_coins(base_key, coin_sat, 1)
      end

    case funds do
      {:ok, funds} ->
        inputs = [s_permission | funds]
        {:ok, c_key} = Wallet.derive_and_insert_key(base_key, s_key, c_dir)

        c_permission_utxo = c_permission_utxo(c_key)

        # is seems first param should be derived key's address
        meta = meta_utxo(c_key.address, content, s_key.dir_txid)

        outputs = [
          meta,
          # reuse self permission
          s_permission | List.duplicate(c_permission_utxo, @permission_num)
        ]

        {change_script, change_pkid} = change_to_address(base_key, opts)

        case handle_change(inputs, outputs, change_script, change_pkid) do
          {:error, msg} ->
            {:error, msg}

          {:ok, inputs, outputs} ->
            {:ok, txid, hex_tx} = make_tx(inputs, outputs, coin_sat)
            Wallet.update_private_key(c_key, %{dir_txid: txid})
            ChatEngine.notify(%{msg_id: c_dir, data: content, txid: txid, time: c_key.inserted_at})
            {:ok, txid, hex_tx}
        end
      {:error, msg} ->
        {:error, msg}
    end

  end

  def make_tx(inputs, outputs, coin_sat) do
    binary_tx = Txmaker.create_p2pkh_transaction(inputs, outputs)

    hex_tx = Binary.to_hex(binary_tx)
    Logger.debug(hex_tx)

    txid = Txmaker.get_txid_from_binary_tx(binary_tx)
    ## TODO save the tx for broadcasting
    {:ok, _} =
      Repo.transaction(fn ->
        # delete inputs
        for u <- inputs, do: Repo.delete!(u)
        # insert outputs
        outs =
          outputs
          |> Stream.map(&set_utxo_type(&1, coin_sat))
          |> Stream.with_index()
          |> Stream.map(fn {x, i} -> Map.put(x, :index, i) |> Map.put(:txid, txid) end)
          |> Enum.map(fn x ->
            %{
              txid: x.txid,
              index: x.index,
              type: x.type,
              value: x.value,
              lock_script: x.lock_script,
              private_key_id: x.private_key_id
            }
          end)

        Repo.insert_all(Utxo, outs)
      end)

    {:ok, txid, hex_tx}
  end

  defp add_change(outputs, change, s, p) do
    if change do
      outputs ++ [%__MODULE__{lock_script: s, value: change, private_key_id: p}]
    else
      outputs
    end
  end

  # decrease outputs until tx valid or outputs empty
  defp get_change_amount(_, []) do
    {:error, "inputs value is too low"}
  end

  defp get_change_amount(inputs, outputs) do
    case Txmaker.get_change(inputs, outputs) do
      :insufficient ->
        Logger.debug("get_change_amount: insufficient")
        get_change_amount(inputs, tl(outputs))

      {:change, change, _inputs, outputs} ->
        Logger.debug("get_change_amount: #{inspect(change)}")
        {change, outputs}

      {:no_change, _inputs, outputs} ->
        {nil, outputs}
    end
  end

  def sum_of_value(utxos) when is_list(utxos) do
    Enum.reduce(utxos, 0, fn x, acc -> Decimal.add(acc, x.value) end)
  end
end
