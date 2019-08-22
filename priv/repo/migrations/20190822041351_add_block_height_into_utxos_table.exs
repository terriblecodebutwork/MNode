defmodule Bex.Repo.Migrations.AddBlockHeightIntoUtxosTable do
  use Ecto.Migration

  def change do
    alter table(:utxos) do
      add :block_height, :integer
    end
  end
end
