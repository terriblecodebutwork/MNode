defmodule Bex.Repo.Migrations.AddTypeIntoUtxosTable do
  use Ecto.Migration
  alias Bex.UtxoType

  def change do
    UtxoType.create_type()
    alter table(:utxos) do
      add :type, UtxoType.type()
    end
  end
end
