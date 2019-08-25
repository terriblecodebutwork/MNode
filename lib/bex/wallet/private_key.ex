defmodule Bex.Wallet.PrivateKey do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.Utxo

  schema "private_keys" do
    field :address, :string
    field :bn, :binary
    field :from, :integer
    field :hex, :string
    field :app_key, :string
    has_many :utxos, Utxo

    timestamps()
  end

  @doc false
  def changeset(private_key, attrs) do
    private_key
    |> cast(attrs, [:hex, :bn, :from, :address, :app_key])
    |> validate_required([:hex, :bn, :from, :address])
  end
end
