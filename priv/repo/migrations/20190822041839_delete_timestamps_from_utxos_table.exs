defmodule Bex.Repo.Migrations.DeleteTimestampsFromUtxosTable do
  use Ecto.Migration

  def change do
    alter table(:utxos) do
      remove :inserted_at
      remove :updated_at
    end
  end
end
