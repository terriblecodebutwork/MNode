defmodule Bex.Repo.Migrations.AddBaseKeyIntoPrivateKeys do
  use Ecto.Migration

  def change do
    alter table(:private_keys) do
      add :base_key_id, references(:private_keys)
    end
  end
end
