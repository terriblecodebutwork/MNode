defmodule Bex.Repo.Migrations.AddAppKeyIntoPrivateKeysTable do
  use Ecto.Migration

  def change do
    alter table(:private_keys) do
      add :app_key, :string
    end
  end
end
