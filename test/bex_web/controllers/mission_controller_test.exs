defmodule BexWeb.MissionControllerTest do
  use BexWeb.ConnCase

  alias Bex.Wallet

  @create_attrs %{txid: "some txid"}
  @update_attrs %{txid: "some updated txid"}
  @invalid_attrs %{txid: nil}

  def fixture(:mission) do
    {:ok, mission} = Wallet.create_mission(@create_attrs)
    mission
  end

  describe "index" do
    test "lists all missions", %{conn: conn} do
      conn = get(conn, Routes.mission_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Missions"
    end
  end

  describe "new mission" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.mission_path(conn, :new))
      assert html_response(conn, 200) =~ "New Mission"
    end
  end

  describe "create mission" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.mission_path(conn, :create), mission: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.mission_path(conn, :show, id)

      conn = get(conn, Routes.mission_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Mission"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.mission_path(conn, :create), mission: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Mission"
    end
  end

  describe "edit mission" do
    setup [:create_mission]

    test "renders form for editing chosen mission", %{conn: conn, mission: mission} do
      conn = get(conn, Routes.mission_path(conn, :edit, mission))
      assert html_response(conn, 200) =~ "Edit Mission"
    end
  end

  describe "update mission" do
    setup [:create_mission]

    test "redirects when data is valid", %{conn: conn, mission: mission} do
      conn = put(conn, Routes.mission_path(conn, :update, mission), mission: @update_attrs)
      assert redirected_to(conn) == Routes.mission_path(conn, :show, mission)

      conn = get(conn, Routes.mission_path(conn, :show, mission))
      assert html_response(conn, 200) =~ "some updated txid"
    end

    test "renders errors when data is invalid", %{conn: conn, mission: mission} do
      conn = put(conn, Routes.mission_path(conn, :update, mission), mission: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Mission"
    end
  end

  describe "delete mission" do
    setup [:create_mission]

    test "deletes chosen mission", %{conn: conn, mission: mission} do
      conn = delete(conn, Routes.mission_path(conn, :delete, mission))
      assert redirected_to(conn) == Routes.mission_path(conn, :index)

      assert_error_sent 404, fn ->
        get(conn, Routes.mission_path(conn, :show, mission))
      end
    end
  end

  defp create_mission(_) do
    mission = fixture(:mission)
    {:ok, mission: mission}
  end
end
