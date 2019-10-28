defmodule BexWeb.Plug do
  import Plug.Conn
  import Phoenix.Controller
  alias Bex.Wallet

  def find_private_key(conn, _options) do
    case get_req_header(conn, "x-app-key") do
      [] ->
        conn
        |> json(%{error: "no X-app-key in headers"})
        |> halt()

      [app_key] ->
        case Wallet.find_private_key_by_app_key(app_key) do
          nil ->
            conn
            |> json(%{error: "X-app-key invalid"})
            |> halt()

          pk ->
            conn
            |> assign(:private_key, pk)
        end

      other ->
        conn
        |> json(%{error: "invalid x-app-key: #{inspect(other)}"})
        |> halt()
    end
  end

  def fetch_onchain_path(conn, _) do
    case get_req_header(conn, "x-onchain-path") do
      [] ->
        conn
        |> json(%{error: "no x-onchain-path in headers"})
        |> halt()

      [path] ->
        if Path.type(path) != :absolute do
          conn
          |> json(%{error: "x-onchain-path must be absolute path"})
          |> halt()
        else
          case action_name(conn) do
            :write ->
              conn
              |> assign(:onchain_path, path)

            :read ->
              conn
              |> assign(:onchain_path, path)

            :mkdir ->
              if Path.extname(path) != "" do
                conn
                |> json(%{error: "'#{path}' is invalid dir name"})
                |> halt()
              else
                conn
                |> assign(:onchain_path, path)
              end
          end
        end

      other ->
        conn
        |> json(%{error: "invalid x-onchain-path: #{inspect(other)}"})
        |> halt()
    end
  end
end
