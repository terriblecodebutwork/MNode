defmodule Bex.Repo.Migrations.CreateUtxos do
  use Ecto.Migration

  def change do
    create table(:utxos) do
      add :value, :decimal
      add :lock_script, :binary
      add :txid, :string
      add :index, :integer

      timestamps()
    end
  end
end
