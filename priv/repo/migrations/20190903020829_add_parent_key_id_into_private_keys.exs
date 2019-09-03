defmodule Bex.Repo.Migrations.AddParentKeyIdIntoPrivateKeys do
  use Ecto.Migration

  def change do
    alter table(:private_keys) do
      add :parent_key_id, references(:private_keys, on_delete: :delete_all)
    end
  end
end
