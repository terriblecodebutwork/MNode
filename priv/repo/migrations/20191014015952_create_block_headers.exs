defmodule Bex.Repo.Migrations.CreateBlockHeaders do
  use Ecto.Migration

  def change do
    create table(:block_headers, primary_key: false) do
      add :id, :integer, primary_key: true

      timestamps()
    end
  end
end
