defmodule Bex.Repo.Migrations.AddBaseKeyIntoDocuments do
  use Ecto.Migration

  def change do
    alter table(:documents) do
      add :base_key_id, references(:private_keys)
    end
  end
end
