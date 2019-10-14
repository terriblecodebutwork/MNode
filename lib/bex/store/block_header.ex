defmodule Bex.Store.BlockHeader do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :integer, autogenerate: false}

  schema "block_headers" do

    timestamps()
  end

  @doc false
  def changeset(block_header, attrs) do
    block_header
    |> cast(attrs, [:id])
    |> validate_required([:id])
  end
end
