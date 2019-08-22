defmodule Bex.Repo.Migrations.CreatePrivateKeys do
  use Ecto.Migration

  def change do
    create table(:private_keys) do
      add :hex, :string
      add :bn, :binary
      add :from, :integer
      add :address, :string

      timestamps()
    end
  end
end
