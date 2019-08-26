defmodule Bex.Wallet.Document do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.PrivateKey
  alias BexLib.Key

  schema "documents" do
    field :dir, :string
    field :filename, :string
    # where the file saved in disk
    field :path, :string
    field :type, :string
    belongs_to :private_key, PrivateKey

    timestamps()
  end

  @doc false
  def changeset(document, attrs) do
    document
    |> cast(attrs, [:path, :type, :filename, :dir])
    |> validate_required([:dir, :type])
  end

  @doc """
  %Document{} -> [binary_key]
  """
  def get_keys_from_dir(doc) do
    doc.dir
    |> get_children_dirs()
    |> Enum.map(fn x -> Key.drive_key(doc.private_key.bn, x) end)
  end

  @doc """
  "a/b/c" -> ["a", "a/b", "a/b/c"]
  """
  def get_children_dirs(dir) do
    dir
    |> String.split(["/"])
    |> Enum.scan(fn x, acc -> Enum.join([acc, x], "/") end)
  end
end
