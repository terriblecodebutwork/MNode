defmodule BexWeb.UtxoControllerTest do
  use BexWeb.ConnCase

  alias Bex.Wallet

  @create_attrs %{index: 42, lock_script: "some lock_script", txid: "some txid", value: "120.5"}
  @update_attrs %{
    index: 43,
    lock_script: "some updated lock_script",
    txid: "some updated txid",
    value: "456.7"
  }
  @invalid_attrs %{index: nil, lock_script: nil, txid: nil, value: nil}

  def fixture(:utxo) do
    {:ok, utxo} = Wallet.create_utxo(@create_attrs)
    utxo
  end

  describe "index" do
    test "lists all utxos", %{conn: conn} do
      conn = get(conn, Routes.utxo_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Utxos"
    end
  end

  describe "new utxo" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.utxo_path(conn, :new))
      assert html_response(conn, 200) =~ "New Utxo"
    end
  end

  describe "create utxo" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.utxo_path(conn, :create), utxo: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.utxo_path(conn, :show, id)

      conn = get(conn, Routes.utxo_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Utxo"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.utxo_path(conn, :create), utxo: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Utxo"
    end
  end

  describe "edit utxo" do
    setup [:create_utxo]

    test "renders form for editing chosen utxo", %{conn: conn, utxo: utxo} do
      conn = get(conn, Routes.utxo_path(conn, :edit, utxo))
      assert html_response(conn, 200) =~ "Edit Utxo"
    end
  end

  describe "update utxo" do
    setup [:create_utxo]

    test "redirects when data is valid", %{conn: conn, utxo: utxo} do
      conn = put(conn, Routes.utxo_path(conn, :update, utxo), utxo: @update_attrs)
      assert redirected_to(conn) == Routes.utxo_path(conn, :show, utxo)

      conn = get(conn, Routes.utxo_path(conn, :show, utxo))
      assert html_response(conn, 200) =~ "some updated txid"
    end

    test "renders errors when data is invalid", %{conn: conn, utxo: utxo} do
      conn = put(conn, Routes.utxo_path(conn, :update, utxo), utxo: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Utxo"
    end
  end

  describe "delete utxo" do
    setup [:create_utxo]

    test "deletes chosen utxo", %{conn: conn, utxo: utxo} do
      conn = delete(conn, Routes.utxo_path(conn, :delete, utxo))
      assert redirected_to(conn) == Routes.utxo_path(conn, :index)

      assert_error_sent 404, fn ->
        get(conn, Routes.utxo_path(conn, :show, utxo))
      end
    end
  end

  defp create_utxo(_) do
    utxo = fixture(:utxo)
    {:ok, utxo: utxo}
  end
end
