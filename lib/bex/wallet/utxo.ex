defmodule Bex.Wallet.Utxo do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.PrivateKey
  alias Bex.Wallet
  alias Bex.UtxoType
  alias BexLib.Txmaker

  schema "utxos" do
    field :index, :integer
    field :lock_script, :binary
    field :txid, :string
    field :value, :decimal
    field :block_height, :integer
    field :type, UtxoType
    belongs_to :private_key, PrivateKey
  end

  @doc false
  def changeset(utxo, attrs) do
    utxo
    |> cast(attrs, [:value, :lock_script, :txid, :index, :type])
    |> validate_required([:value, :lock_script, :txid, :index, :type])
  end

  @coin_sat Decimal.cast(100_000)

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
  Return {:ok, new_utxos} or {:error, msg}
  """
  def mint(u = %__MODULE__{lock_script: s, value: v, private_key_id: p}) do
    inputs = [u]
    coin_num = Decimal.div_int(v, @coin_sat) |> Decimal.to_integer()
    coin_utxo = %__MODULE__{value: @coin_sat, private_key_id: p, lock_script: s}
    outputs = List.duplicate(coin_utxo, coin_num)
    change = get_change_amount(inputs, outputs)
    outputs = add_change(outputs, change, s)

    bn = Wallet.get_private_key!(p).bn
    Txmaker.create_p2pkh_transaction(bn, inputs, outputs) |> IO.inspect()
    ## TODO
  end

  defp add_change(outputs, change, s) do
    if change do
      [%__MODULE__{lock_script: s, value: change} | outputs]
    else
      outputs
    end
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
