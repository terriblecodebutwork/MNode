defmodule Bex.Store.Merkle do
  use Ecto.Schema
  import Ecto.Changeset
  alias __MODULE__

  @primary_key {:id, :string, autogenerate: false}
  @foreign_key_type :string

  schema "merkle" do
    field :block_height, :integer
    field :root, :boolean
    belongs_to :pair, Merkle, foreign_key: :pair_id
    belongs_to :top, Merkle, foreign_key: :top_id

    timestamps()
  end

  @doc false
  def changeset(merkle, attrs) do
    merkle
    |> cast(attrs, [:id, :block_height, :pair_id, :top_id, :root])
    |> validate_required([:id, :block_height])
  end
end
