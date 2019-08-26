defmodule Bex.Repo.Migrations.CreateDocuments do
  use Ecto.Migration

  def change do
    create table(:documents) do
      add :path, :string
      add :type, :string
      add :filename, :string
      add :dir, :string
      add :private_key_id, references(:private_keys, on_delete: :nothing)

      timestamps()
    end

    create index(:documents, [:private_key_id])
  end
end
