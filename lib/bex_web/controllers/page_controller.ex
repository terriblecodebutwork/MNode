defmodule BexWeb.PageController do
  use BexWeb, :controller
  require Logger

  alias Phoenix.LiveView
  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.Txrepo

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def show_tx(conn, _params) do
    data = Txrepo.list() |> inspect()
    text(conn, "正在重试的交易\n" <> data)
  end

  def ad(conn, _params) do
    conn
    |> load_key_id_from_cookie()
    |> render_live(BexWeb.AdLive)
  end

  def chat(conn, _params) do
    conn
    |> load_key_id_from_cookie()
    |> render_live(BexWeb.ChatLive)
  end

  defp load_key_id_from_cookie(conn) do
    case get_session(conn, "key") do
      nil ->
        priv = Key.new_private_key() |> Binary.to_hex()
        {:ok, key} = Wallet.create_private_key(%{"hex" => priv})
        put_session(conn, "key", key.id)

      key ->
        Logger.debug("key: #{key}")
        conn
    end
  end

  defp render_live(conn, live) do
    LiveView.Controller.live_render(conn, live, session: %{key: get_session(conn, "key")})
  end

  def gun(conn, _params) do
    conn =
      case get_session(conn) do
        %{"key" => key, "key2" => key2} ->
          Logger.debug("key: #{key}; key2: #{key2}")
          conn

        %{"key" => _key} ->
          priv = Key.new_private_key() |> Binary.to_hex()
          {:ok, key2} = Wallet.create_private_key(%{"hex" => priv})

          conn
          |> put_session("key2", key2.id)

        _ ->
          priv = Key.new_private_key() |> Binary.to_hex()
          {:ok, key} = Wallet.create_private_key(%{"hex" => priv})

          priv = Key.new_private_key() |> Binary.to_hex()
          {:ok, key2} = Wallet.create_private_key(%{"hex" => priv})

          conn
          |> put_session("key", key.id)
          |> put_session("key2", key2.id)
      end

    LiveView.Controller.live_render(conn, BexWeb.GunLive,
      session: %{key: get_session(conn, "key"), key2: get_session(conn, "key2")}
    )
  end

  def merkle_path(conn, %{"txid" => t}) do
    path = Bex.Store.get_merkle_path(t)

    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> json(%{merkle_path: path})
  end

  def merkle_page(conn, _) do
    render(conn, "merkle.html")
  end
end
