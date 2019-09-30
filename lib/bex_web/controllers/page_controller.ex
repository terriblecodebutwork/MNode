defmodule BexWeb.PageController do
  use BexWeb, :controller
  require Logger

  alias Phoenix.LiveView
  alias BexLib.Key
  alias Bex.Wallet

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def ad(conn, _params) do
    conn =
      case get_session(conn, "key") do
        nil ->
          priv = Key.new_private_key() |> Binary.to_hex()
          {:ok, key} = Wallet.create_private_key(%{"hex" => priv})
          put_session(conn, "key", key.id)
        key ->
          Logger.debug "key: #{key}"
          conn
      end
    LiveView.Controller.live_render(conn, BexWeb.AdLive, session: %{key: get_session(conn, "key")})
  end

  def gun(conn, _params) do
    conn =
      case get_session(conn) do
        %{"key" => key, "key2" => key2} ->
          Logger.debug "key: #{key}; key2: #{key2}"
          conn

        %{"key" => key} ->
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
    LiveView.Controller.live_render(conn, BexWeb.GunLive, session: %{key: get_session(conn, "key"), key2: get_session(conn, "key2")})
  end
end
