defmodule BexWeb.PrivateKeyControllerTest do
  use BexWeb.ConnCase

  alias Bex.Wallet

  @create_attrs %{address: "some address", bn: "some bn", from: 42, hex: "some hex"}
  @update_attrs %{
    address: "some updated address",
    bn: "some updated bn",
    from: 43,
    hex: "some updated hex"
  }
  @invalid_attrs %{address: nil, bn: nil, from: nil, hex: nil}

  def fixture(:private_key) do
    {:ok, private_key} = Wallet.create_private_key(@create_attrs)
    private_key
  end

  describe "index" do
    test "lists all private_keys", %{conn: conn} do
      conn = get(conn, Routes.private_key_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Private keys"
    end
  end

  describe "new private_key" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.private_key_path(conn, :new))
      assert html_response(conn, 200) =~ "New Private key"
    end
  end

  describe "create private_key" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.private_key_path(conn, :create), private_key: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.private_key_path(conn, :show, id)

      conn = get(conn, Routes.private_key_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Private key"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.private_key_path(conn, :create), private_key: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Private key"
    end
  end

  describe "edit private_key" do
    setup [:create_private_key]

    test "renders form for editing chosen private_key", %{conn: conn, private_key: private_key} do
      conn = get(conn, Routes.private_key_path(conn, :edit, private_key))
      assert html_response(conn, 200) =~ "Edit Private key"
    end
  end

  describe "update private_key" do
    setup [:create_private_key]

    test "redirects when data is valid", %{conn: conn, private_key: private_key} do
      conn =
        put(conn, Routes.private_key_path(conn, :update, private_key), private_key: @update_attrs)

      assert redirected_to(conn) == Routes.private_key_path(conn, :show, private_key)

      conn = get(conn, Routes.private_key_path(conn, :show, private_key))
      assert html_response(conn, 200) =~ "some updated address"
    end

    test "renders errors when data is invalid", %{conn: conn, private_key: private_key} do
      conn =
        put(conn, Routes.private_key_path(conn, :update, private_key), private_key: @invalid_attrs)

      assert html_response(conn, 200) =~ "Edit Private key"
    end
  end

  describe "delete private_key" do
    setup [:create_private_key]

    test "deletes chosen private_key", %{conn: conn, private_key: private_key} do
      conn = delete(conn, Routes.private_key_path(conn, :delete, private_key))
      assert redirected_to(conn) == Routes.private_key_path(conn, :index)

      assert_error_sent 404, fn ->
        get(conn, Routes.private_key_path(conn, :show, private_key))
      end
    end
  end

  defp create_private_key(_) do
    private_key = fixture(:private_key)
    {:ok, private_key: private_key}
  end
end
