defmodule Bex.Repo.Migrations.AddSpendMissionIdAndCreateMissionIdIntoUtxosTable do
  use Ecto.Migration

  def change do
    alter table(:utxos) do
      add :consumer_id, references(:missions)
      add :producer_id, references(:missions)
    end
  end
end
