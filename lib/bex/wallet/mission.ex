defmodule Bex.Wallet.Mission do
  use Ecto.Schema
  import Ecto.Changeset
  alias Bex.MissionStatus
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.Document

  schema "missions" do
    field :txid, :string
    field :status, MissionStatus, default: :offchain
    field :v_outputs, :string, virtual: true
    has_many :inputs, Utxo, foreign_key: :consumer_id
    has_many :outputs, Utxo, foreign_key: :producer_id
    belongs_to :document, Document

    timestamps()
  end

  @doc false
  def changeset(mission, attrs) do
    mission
    |> cast(attrs, [:txid, :status, :document_id])
    |> cast_assoc(:inputs)
    |> cast_assoc(:outputs)
    |> validate_required([:status, :inputs, :outputs])
  end

  def add_v_output(%__MODULE__{v_outputs: v}=m, output) do
    %{m | v_outputs: v ++ [output]}
  end

end
