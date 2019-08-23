defmodule Bex.Wallet.Mission do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.MissionType
  alias Bex.MissionStatus
  alias Bex.Wallet.Utxo

  schema "missions" do
    field :txid, :string
    field :type, MissionType
    field :status, MissionStatus, default: :offchain
    has_many :inputs, Utxo, foreign_key: :consumer_id
    has_many :outputs, Utxo, foreign_key: :producer_id

    timestamps()
  end

  @doc false
  def changeset(mission, attrs) do
    mission
    |> cast(attrs, [:txid, :type, :status])
    |> validate_required([:type, :status])
  end
end
