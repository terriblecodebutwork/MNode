defmodule Bex.Repo.Migrations.AddPrivateKeyToUtxosTable do
  use Ecto.Migration

  def change do
    alter table(:utxos) do
      add :private_key_id, references(:private_keys)
    end
  end
end
