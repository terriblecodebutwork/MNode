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
    case get_session(conn, "key") do
      nil ->
        priv = Key.new_private_key() |> Binary.to_hex()
        {:ok, key} = Wallet.create_private_key(%{"hex" => priv})
        put_session(conn, "key", key.id)
      key ->
        Logger.debug "key: #{key}"
        conn
    end
    |> LiveView.Controller.live_render(BexWeb.AdLive, session: %{key: get_session(conn, "key")})
  end
end
