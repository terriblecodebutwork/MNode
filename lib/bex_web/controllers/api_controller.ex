defmodule BexWeb.ApiController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.Utxo
  require Logger

  plug :find_private_key

  @doc """
  Create a metanet directory or file.
  The private key is association with the APP_KEY in the header.

  params:
    dir: "a/b/c"
    file: %Plug.Upload{}

  if only dir, create a dir; if path and file, creat the file.
  Can not create dir or file under unexisted dir.
  """
  def create(conn, %{"path" => dir} = params) do
    base_key = conn.assigns.private_key
    Logger.info("create_root_dir: #{inspect(dir)}")

    content =
      case params["content"] do
        b when is_binary(b) ->
          [b]
        m when is_map(m) ->
          IO.inspect m
          m
          |> Enum.map(fn {k, v} ->
            i = String.to_integer(k)
            {i, v}
          end)
          |> Enum.sort()
          |> Enum.map(fn {_, v} -> v end)
        _ ->
          []
      end

    case dir_type(dir) do
      :root ->
        {:ok, _txid, hex_tx} = Utxo.create_root_dir(base_key, dir, content)
        json(conn, %{code: 0, raw_tx: "#{hex_tx}"})
      :noroot ->
        case Wallet.find_key_with_dir(base_key, Path.dirname(dir)) do
          {:ok, s_key} ->
            IO.inspect content
            {:ok, _txid, hex_tx} = Utxo.create_sub_dir(s_key, dir, content)
            json(conn, %{code: 0, raw_tx: "#{hex_tx}"})
          {:error, _} -> json(conn, %{code: 1, error: "mnode: #{dir}: No such file or directory"})
        end
    end
  end

  def create(conn, _) do
    json(conn, %{error: "need dir"})
  end

  def dir_type(dir) do
    case String.contains?(dir, "/") do
      true -> :noroot
      false -> :root
    end
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
        |> json(%{error: "invalid APP_KEY: #{inspect(other)}"})
        |> halt()
    end
  end
end
