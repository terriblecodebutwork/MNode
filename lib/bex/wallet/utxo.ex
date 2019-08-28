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
  alias __MODULE__
  # alias Bex.Wallet.Mission

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
    |> validate_required([:value, :lock_script,  :type])
  end

  @coin_sat Decimal.cast(10_000)

  def set_utxo_type(utxo = %{value: v}) do
    type =
      case Decimal.cmp(v, @coin_sat) do
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
    inputs = [u]
    coin_num = Decimal.div_int(v, @coin_sat) |> Decimal.to_integer()
    coin_utxo = %__MODULE__{value: @coin_sat, private_key_id: pkid, lock_script: s}
    outputs = List.duplicate(coin_utxo, coin_num)
    case get_change_amount(inputs, outputs) do
      {:error, msg} ->
        {:error, msg}
      other ->
        outputs = add_change(outputs, other, s, pkid)

        pk = Wallet.get_private_key!(pkid)

        make_tx(pk, inputs, outputs)
    end
  end

  @doc """
  Combian all dust of a private key into the coins.
  """
  def recast(%PrivateKey{} = p) do
    dusts =
      (from u in Utxo,
      where: u.private_key_id == ^p.id and u.type == "dust")
      |> Repo.all()
    inputs = dusts
    inputs_value = sum_of_value(inputs)
    coin_num = Decimal.div_int(inputs_value, @coin_sat) |> Decimal.to_integer()
    if coin_num == 0 do
      {:error, "not enough dusts"}
    else
      s = Key.private_key_to_p2pkh_script(p.bn)
      coin_utxo = %__MODULE__{value: @coin_sat, private_key_id: p.id, lock_script: s}
      outputs = List.duplicate(coin_utxo, coin_num)
      case get_change_amount(inputs, outputs) do
        {:error, msg} ->
          {:error, msg}
        other ->
          outputs = add_change(outputs, other, s, p.id)
          make_tx(p, inputs, outputs)
      end
    end
  end

  def make_tx(pk, inputs, outputs) do
    bn = pk.bn
    binary_tx = Txmaker.create_p2pkh_transaction(bn, inputs, outputs)
    txid = Txmaker.get_txid_from_binary_tx(binary_tx)
    ## TODO save the tx for broadcasting
    Repo.transaction(fn ->
      # delete inputs
      for u <- inputs, do: Repo.delete!(u)
      # insert outputs
      outputs
      |> Stream.map(&set_utxo_type/1)
      |> Stream.with_index()
      |> Stream.map(fn {x, i} -> Map.put(x, :index, i) |> Map.put(:txid, txid) end)
      |> Enum.map(&Repo.insert!/1)
    end)
  end

  defp add_change(outputs, change, s, p) do
    if change do
      [%__MODULE__{lock_script: s, value: change, private_key_id: p} | outputs]
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
        get_change_amount(inputs, tl(outputs))

      {:change, change, _} ->
        change

      {:nochange, _} ->
        nil
    end
  end

  def sum_of_value(utxos) when is_list(utxos) do
    Enum.reduce(utxos, 0, fn x, acc -> Decimal.add(acc, x.value) end)
  end
end
