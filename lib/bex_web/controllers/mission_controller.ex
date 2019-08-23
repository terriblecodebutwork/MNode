defmodule BexWeb.MissionController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.Mission

  def index(conn, _params) do
    missions = Wallet.list_missions()
    render(conn, "index.html", missions: missions)
  end

  def new(conn, _params) do
    changeset = Wallet.change_mission(%Mission{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"mission" => mission_params}) do
    case Wallet.create_mission(mission_params) do
      {:ok, mission} ->
        conn
        |> put_flash(:info, "Mission created successfully.")
        |> redirect(to: Routes.mission_path(conn, :show, mission))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    mission = Wallet.get_mission!(id)
    render(conn, "show.html", mission: mission)
  end

  def edit(conn, %{"id" => id}) do
    mission = Wallet.get_mission!(id)
    changeset = Wallet.change_mission(mission)
    render(conn, "edit.html", mission: mission, changeset: changeset)
  end

  def update(conn, %{"id" => id, "mission" => mission_params}) do
    mission = Wallet.get_mission!(id)

    case Wallet.update_mission(mission, mission_params) do
      {:ok, mission} ->
        conn
        |> put_flash(:info, "Mission updated successfully.")
        |> redirect(to: Routes.mission_path(conn, :show, mission))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", mission: mission, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    mission = Wallet.get_mission!(id)
    {:ok, _mission} = Wallet.delete_mission(mission)

    conn
    |> put_flash(:info, "Mission deleted successfully.")
    |> redirect(to: Routes.mission_path(conn, :index))
  end
end
