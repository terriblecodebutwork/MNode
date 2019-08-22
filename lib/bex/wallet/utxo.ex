defmodule Bex.Wallet.Utxo do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.PrivateKey

  schema "utxos" do
    field :index, :integer
    field :lock_script, :binary
    field :txid, :string
    field :value, :decimal
    field :block_height, :integer
    belongs_to :private_key, PrivateKey
  end

  @doc false
  def changeset(utxo, attrs) do
    utxo
    |> cast(attrs, [:value, :lock_script, :txid, :index])
    |> validate_required([:value, :lock_script, :txid, :index])
  end
end
