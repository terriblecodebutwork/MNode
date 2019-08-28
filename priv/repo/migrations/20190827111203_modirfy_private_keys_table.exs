defmodule Bex.Repo.Migrations.ModirfyPrivateKeysTable do
  use Ecto.Migration

  def change do
    alter table(:private_keys) do
      remove :from
      add :dir, :string
    end
  end
end
