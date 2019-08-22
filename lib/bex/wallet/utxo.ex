defmodule Bex.Wallet.Utxo do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.PrivateKey
  alias Bex.UtxoType

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
    |> cast(attrs, [:value, :lock_script, :txid, :index])
    |> validate_required([:value, :lock_script, :txid, :index])
  end

  @coin_sat Decimal.cast(100_000)

  def set_utxo_type(utxo = %{value: v}) do
    type = case Decimal.cmp(v, @coin_sat) do
      :lt -> :dust
      :eq -> :coin
      :gt -> :gold
    end
    Map.put(utxo, :type, type)
  end

end
