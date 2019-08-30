defmodule Bex.Wallet.PrivateKey do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.Document
  alias BexLib.Key
  alias Bex.Repo
  import Ecto.Query
  alias __MODULE__

  schema "private_keys" do
    field :address, :string
    field :bn, :binary
    # the dir which derived this key
    field :dir, :string
    field :hex, :string
    field :app_key, :string
    belongs_to :base_key, PrivateKey, foreign_key: :base_key_id
    has_many :utxos, Utxo
    has_many :documents, Document, foreign_key: :private_key_id
    has_many :derive_keys, PrivateKey, foreign_key: :base_key_id

    timestamps()
  end

  @doc false
  def changeset(private_key, attrs) do
    private_key
    |> cast(attrs, [:hex, :bn, :dir, :address, :app_key, :base_key_id])
    |> cast_assoc(:base_key)
    |> validate_required([:hex, :bn, :address])
  end

  def hex_changeset(%{hex: hex} = attrs) do
    bn = Base.decode16!(hex, case: :mixed)

    attrs =
      Map.merge(
        %{
          hex: hex,
          bn: bn,
          address: Key.private_key_to_address(bn),
          app_key: :crypto.strong_rand_bytes(32) |> Base.encode64()
        },
        attrs
      )

    %PrivateKey{}
    |> changeset(attrs)
  end

  def derive_changeset(base_key, dir) do
    new_bn = Key.derive_key(base_key.bn, dir)

    hex_changeset(%{hex: Binary.to_hex(new_bn), dir: dir, base_key_id: base_key.id})
  end

  def get_derive_keys_by_id(id) do
    query =
      from p in PrivateKey,
        where: p.base_key_id == ^id and not is_nil(p.dir)

    Repo.all(query)
  end
end
