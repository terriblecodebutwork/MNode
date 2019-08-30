# defmodule Bex.Wallet.Mission do
#   @moduledoc """
#   The Mission table is too complicate now,
#   so I decided not to use this.

#   We just build the tx and save the latest
#   utxos into db. And broadcast the tx.

#   We not save the tx.
#   """

#   use Ecto.Schema
#   import Ecto.Changeset
#   alias Bex.MissionStatus
#   alias Bex.Wallet.Utxo
#   alias Bex.Wallet.Document

#   schema "missions" do
#     field :txid, :string
#     field :status, MissionStatus, default: :offchain
#     has_many :inputs, Utxo, foreign_key: :consumer_id
#     has_many :outputs, Utxo, foreign_key: :producer_id
#     belongs_to :document, Document

#     timestamps()
#   end

#   @doc false
#   def changeset(mission, attrs) do
#     mission
#     |> cast(attrs, [:txid, :status, :document_id])
#     |> cast_assoc(:inputs)
#     |> cast_assoc(:outputs)
#     |> validate_required([:status, :inputs, :outputs])
#   end

# end
