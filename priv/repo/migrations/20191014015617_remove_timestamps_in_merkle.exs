defmodule Bex.Repo.Migrations.RemoveTimestampsInMerkle do
  use Ecto.Migration

  def change do
    alter table(:merkle) do
      remove :inserted_at
      remove :updated_at
    end
  end
end
