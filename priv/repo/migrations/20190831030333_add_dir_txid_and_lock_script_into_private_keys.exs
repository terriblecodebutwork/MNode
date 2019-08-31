defmodule Bex.Repo.Migrations.AddDirTxidAndLockScriptIntoPrivateKeys do
  use Ecto.Migration

  def change do
    alter table(:private_keys) do
      add :dir_txid, :string
      add :lock_script, :binary
    end
  end
end
