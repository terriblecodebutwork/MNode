defmodule Bex.Repo.Migrations.AddDocumentIdIntoTheMissionsTable do
  use Ecto.Migration

  def change do
    alter table(:missions) do
      add :document_id, references(:documents)
    end
  end
end
