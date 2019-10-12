defmodule Bex.Repo.Migrations.CreateMerkle do
  use Ecto.Migration

  def up do
    create table(:merkle, primary_key: false) do
      add :id, :string, size: 64, primary_key: true
      add :block_height, :integer
      add :top_id, references(:merkle, type: :string)
      add :pair_id, references(:merkle, type: :string)
      add :root, :boolean
      add :at_left, :boolean

      timestamps()
    end

    execute "ALTER TABLE merkle DISABLE TRIGGER ALL;"
  end

  def down do
    drop table(:merkle)
  end
end
