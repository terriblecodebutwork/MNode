defmodule Bex.Repo.Migrations.CreateMissions do
  use Ecto.Migration
  alias Bex.MissionType
  alias Bex.MissionStatus

  def change do
    MissionType.create_type()
    MissionStatus.create_type()

    create table(:missions) do
      add :txid, :string
      add :type, MissionType.type()
      add :status, MissionStatus.type()
      timestamps()
    end
  end
end
