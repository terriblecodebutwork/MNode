defmodule Bex.Repo.Migrations.CreateMissions do
  use Ecto.Migration
  alias Bex.MissionStatus

  def change do
    MissionStatus.create_type()

    create table(:missions) do
      add :txid, :string
      add :status, MissionStatus.type()
      timestamps()
    end
  end
end
