defmodule BexWeb.ApiController do
  use BexWeb, :controller

  alias Bex.Wallet
  require Logger

  plug :find_private_key

  @doc """
  Create a metanet directory or file.
  The private key is association with the APP_KEY in the header.

  params:
    path: "a/b/c"
    file: %Plug.Upload{}

  if only path, create a dir; if path and file, creat the file.
  Can not create dir or file under unexisted dir.
  """
  def create(conn, params) do
    Logger.debug inspect conn.assigns.private_key
    text(conn, "ok")
  end

  # {:error, msg} or {:ok, private_key}
  defp find_private_key(conn, _options) do
    case get_req_header(conn, "app_key") do
      [] ->
        conn
        |> json(%{error: "no app_key in headers"})
        |> halt()
      [app_key] ->
        case Wallet.find_private_key_by_app_key(app_key) do
          nil ->
            conn
            |> json(%{error: "app_key not exists"})
            |> halt()
          pk ->
            conn
            |> assign(:private_key, pk)
        end
      other ->
        conn
        |> json(%{error: "invalid APP_KEY: #{inspect other}"})
        |> halt()
    end
  end

end