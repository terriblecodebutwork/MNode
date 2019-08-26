defmodule Bex.Repo.Migrations.ModifyReferencesOfUtxosAndDocumentsTalbe do
  use Ecto.Migration

  def up do
    execute "ALTER TABLE utxos DROP CONSTRAINT utxos_private_key_id_fkey"

    alter table(:utxos) do
      modify :private_key_id, references(:private_keys, on_delete: :delete_all)
    end

    execute "ALTER TABLE documents DROP CONSTRAINT documents_private_key_id_fkey"

    alter table(:documents) do
      modify :private_key_id, references(:private_keys, on_delete: :delete_all)
    end
  end

  def down do
    execute "ALTER TABLE utxos DROP CONSTRAINT utxos_private_key_id_fkey"

    alter table(:utxos) do
      modify :private_key_id, references(:private_keys, on_delete: :nothing)
    end

    execute "ALTER TABLE documents DROP CONSTRAINT documents_private_key_id_fkey"

    alter table(:documents) do
      modify :private_key_id, references(:private_keys, on_delete: :nothing)
    end
  end
end
