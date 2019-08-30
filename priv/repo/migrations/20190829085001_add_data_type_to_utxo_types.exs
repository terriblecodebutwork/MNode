defmodule Bex.Repo.Migrations.AddDataTypeToUtxoTypes do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    Ecto.Migration.execute "ALTER TYPE utxo_type ADD VALUE IF NOT EXISTS'data'"
  end

  def down do
  end
end

