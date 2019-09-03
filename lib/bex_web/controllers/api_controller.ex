defmodule BexWeb.ApiController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.Utxo
  alias BexLib.Bitindex
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
  def create(conn, %{"parent" => false, "name" => c_dir} = params) do
    c_dir = to_string(c_dir)
    base_key = conn.assigns.private_key
    content = deal_with_content(params)
    {:ok, txid, hex_tx} = Utxo.create_root_dir(base_key, c_dir, content)
    broadcast(conn, params, hex_tx, txid)
  end

  # It's a bit confusing, cause the different view of nodes.
  # In old code, we see the "parent" node as self node, and
  # "id" node as child node.
  def create(conn, %{"parent" => s_dir, "name" => c_dir} = params) do
    s_dir = to_string(s_dir)
    c_dir = to_string(c_dir)
    base_key = conn.assigns.private_key
    content = deal_with_content(params)
    # use parent id and self id as dir, and need the root dir
    case Wallet.find_key_with_dir(base_key, s_dir) do
      {:ok, s_key} ->
        {:ok, txid, hex_tx} = Utxo.create_sub_dir(s_key, c_dir, content)
        broadcast(conn, params, hex_tx, txid)

      {:error, _} ->
        json(conn, %{code: 1, error: "mnode: #{s_dir}: No such file or directory"})
    end
  end

  def create(conn, %{"path" => dir} = params) when is_binary(dir) do
    base_key = conn.assigns.private_key
    Logger.info("create_root_dir: #{inspect(dir)}")
    content = deal_with_content(params)

    case dir_type(dir) do
      :root ->
        {:ok, txid, hex_tx} = Utxo.create_root_dir(base_key, dir, content)
        broadcast(conn, params, hex_tx, txid)

      :noroot ->
        case Wallet.find_key_with_dir(base_key, Path.dirname(dir)) do
          {:ok, s_key} ->
            {:ok, txid, hex_tx} = Utxo.create_sub_dir(s_key, dir, content)
            broadcast(conn, params, hex_tx, txid)

          {:error, _} ->
            json(conn, %{code: 1, error: "mnode: #{dir}: No such file or directory"})
        end
    end
  end

  def create(conn, _) do
    json(conn, %{error: "need dir"})
  end

  defp broadcast(conn, params, hex_tx, txid) do
    if params["broadcast"] == true do
      case Bitindex.broadcast_hex_tx(hex_tx) do
        {:ok, msg} ->
          json(conn, %{code: 0, raw_tx: hex_tx, txid: txid, msg: msg})

        {:error, msg} ->
          json(conn, %{code: 1, error: "mnode: #{inspect(msg)}"})
      end
    else
      json(conn, %{code: 0, raw_tx: hex_tx, txid: txid})
    end
  end

  defp deal_with_content(params) do
    case params["content"] do
      b when is_binary(b) ->
        [b]

      m when is_map(m) ->
        m
        |> Enum.map(fn {k, v} ->
          i = String.to_integer(k)
          {i, v}
        end)
        |> Enum.sort()
        |> Enum.map(fn {_, v} -> v end)

      l when is_list(l) ->
        l

      _ ->
        []
    end
  end

  def find(conn, %{"name" => dir}) do
    base_key = conn.assigns.private_key

    case Wallet.find_txids_with_dir(base_key, dir) do
      {:ok, txids} ->
        json(conn, %{code: 0, txids: txids})

      {:error, _} ->
        json(conn, %{code: 1, error: "mnode: #{dir}: No such file or directory"})
    end
  end

  # def find(conn, %{"path" => dir}) do
  #   base_key = conn.assigns.private_key

  #   case Wallet.find_txids_with_dir(base_key, dir) do
  #     {:ok, txids} ->
  #       json(conn, %{code: 0, txids: txids})

  #     {:error, _} ->
  #       json(conn, %{code: 1, error: "mnode: #{dir}: No such file or directory"})
  #   end
  # end

  defp dir_type(dir) do
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
