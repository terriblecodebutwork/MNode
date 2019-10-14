defmodule Bex.Repo.Migrations.RemoveTimestampsInMerkle do
  use Ecto.Migration

  def up do
    alter table(:merkle) do
      remove :inserted_at
      remove :updated_at
    end
  end

  def down do
    # can not rollback
  end
end
