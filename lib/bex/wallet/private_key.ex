defmodule Bex.Wallet.PrivateKey do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.Document

  schema "private_keys" do
    field :address, :string
    field :bn, :binary
    field :dir, :string # the dir which derived this key
    field :hex, :string
    field :app_key, :string
    has_many :utxos, Utxo
    has_many :documents, Document, foreign_key: :base_key_id
    has_many :derive_documents, Document, foreign_key: :private_key_id

    timestamps()
  end

  @doc false
  def changeset(private_key, attrs) do
    private_key
    |> cast(attrs, [:hex, :bn, :dir, :address, :app_key])
    |> validate_required([:hex, :bn,  :address])
  end
end
