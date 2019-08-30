defmodule Bex.Wallet.Document do
  @moduledoc """
  Document is the unit of the onchain filesystem, the directory is a
  special file. To create a dir or file onchain, we need change the
  Document to one or more Missions. And when the change happening, we
  need the permission utxo of the father dir.
  The permission is a special utxo, value is 546 satoshi(minimum amount
  of a spendable utxo), the permission can be reused by be added into
  the outputs of the mission. The permission is from the address of the
  father dir's creation tx. The private key of the permission is
  generated by: Key.drive_key(base_private_key, dir).
  Every document's dir is the abosulte path, for example: dir "a/b/c"'s
  father dir is "a/b", and grandpa dir is "a".

  This filesystem is not the most felxable fs, because everyone who get
  the base private key can have permission to create any dir and file
  under the tree that built with the same base private key.
  But it's enough for the API-based metanet filesystem, because one
  APP_KEY corresponds to one base private-key.
  """
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.Wallet.PrivateKey
  # alias Bex.Wallet.Mission
  alias BexLib.Key
  alias Bex.Wallet

  schema "documents" do
    field :dir, :string
    field :filename, :string
    # where the file saved in disk
    field :path, :string
    field :type, :string
    belongs_to :private_key, PrivateKey
    belongs_to :base_key, PrivateKey, foreign_key: :base_key_id
    # has_many :missions, Mission

    timestamps()
  end

  @doc false
  def changeset(document, attrs) do
    document
    |> cast(attrs, [:path, :type, :filename, :dir, :private_key_id, :base_key_id])
    |> validate_required([:dir, :type])
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
