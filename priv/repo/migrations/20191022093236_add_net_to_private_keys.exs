defmodule Bex.Repo.Migrations.AddNetToPrivateKeys do
  use Ecto.Migration

  def change do
    alter(table("private_keys")) do
      add :net, :string
    end
  end
end
