defmodule BexWeb.WriteController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.Utxo
  alias BexLib.Bitindex
  alias Bex.CoinManager
  alias Bex.Util
  alias Bex.MetaNode
  alias BexLib.Key
  alias Bex.KV
  require Logger

  plug :find_private_key
  plug :fetch_onchain_path

  defp find_private_key(conn, _options) do
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

  defp fetch_onchain_path(conn, _) do
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

  @doc """
  Create a metanet directory or file.
  The private key is association with the APP_KEY in the header.

  params:
    dir: "a/b/c"
    file: %Plug.Upload{}

  if only dir, create a dir; if path and file, creat the file.
  Can not write dir or file under unexisted dir.
  """
  # def create(conn, %{"parent" => false, "name" => c_dir} = params) do
  #   c_dir = to_string(c_dir)
  #   base_key = conn.assigns.private_key
  #   content = deal_with_content(params)
  #   {:ok, txid, hex_tx} = CoinManager.create_mnode(base_key.id, false, c_dir, content)
  #   respond(conn, params, hex_tx, txid)
  # end

  # # It's a bit confusing, cause the different view of nodes.
  # # In old code, we see the "parent" node as self node, and
  # # "id" node as child node.
  # def create(conn, %{"parent" => s_dir, "name" => c_dir} = params) do
  #   s_dir = to_string(s_dir)
  #   c_dir = to_string(c_dir)
  #   base_key = conn.assigns.private_key
  #   content = deal_with_content(params)
  #   # use parent id and self id as dir, and need the root dir
  #   case CoinManager.create_mnode(base_key.id, s_dir, c_dir, content) do
  #     {:ok, txid, hex_tx} ->
  #       respond(conn, params, hex_tx, txid)

  #     {:error, _} ->
  #       json(conn, %{code: 1, error: "mnode: #{s_dir}: No such file or directory"})
  #   end
  # end

  # def create(conn, %{"path" => path} = params) do
  #   {parent, name} = Util.path_to_name(path)
  #   create(conn, Map.merge(params, %{"parent" => parent, "name" => name}))
  # end

  @chunk_size 80_000
  @read_option [read_length: @chunk_size, length: 1_000]

  defp bcat_part(data, keyid) do
    content = ["1ChDHzdd1H4wSjgGMHyndZm6qxEDGjqpJL", data]
    {:ok, txid, _} = CoinManager.send_opreturn(keyid, content)
    txid
  end

  defp multi_bcat(body, keyid, result) do
    case Binary.split_at(body, @chunk_size) do
      {data, <<>>} ->
        [bcat_part(data, keyid) | result]
      {data, body} ->
        multi_bcat(body, keyid, [bcat_part(data, keyid) | result])
    end
  end

  defp read_remain_part(conn, txids, keyid) do
    len = @chunk_size
    case read_body(conn, @read_option) do
      {:ok, body, _} ->
        if byte_size(body) > len do
          Enum.reverse(multi_bcat(body, keyid, []) ++ txids)
        else
          Enum.reverse([bcat_part(body, keyid) | txids])
        end
      {:more, body, conn} ->
        txids = [bcat_part(body, keyid) | txids]
        read_remain_part(conn, txids, keyid)
    end
  end

  def write(conn, %{}) do
    base_key = conn.assigns.private_key
    onchain_path = conn.assigns.onchain_path
    filename = Path.basename(onchain_path)
    type = MIME.from_path(filename)
    dir = Path.dirname(onchain_path)

    content =
      case read_body(conn, @read_option) do
        {:ok, data, _conn} ->
          # if file_size <= 90kb, use b://
          ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", data, type, "binary", filename]


        {:more, data, conn} ->
          txids = read_remain_part(conn, [bcat_part(data, base_key.id)], base_key.id) |> Enum.map(&Binary.from_hex/1)
          ["15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up", " ", type, "binary", filename, " "] ++ txids
      end

    case CoinManager.create_mnode(base_key.id, dir, onchain_path, content) do
      {:ok, txid, hex_tx} ->
        respond(conn, nil, hex_tx, txid)

      {:error, msg} ->
        json(conn, %{code: 1, error: msg})
    end
  end

  ## root node: "/"
  def mkdir(conn, %{}) do
    base_key = conn.assigns.private_key
    onchain_path = conn.assigns.onchain_path

    dir =
      if onchain_path == "/" do
        false
      else
        Path.dirname(onchain_path)
      end

    case CoinManager.create_mnode(base_key.id, dir, onchain_path, [Path.basename(onchain_path)]) do
      {:ok, txid, hex_tx} ->
        respond(conn, nil, hex_tx, txid)

      {:error, msg} ->
        json(conn, %{code: 1, error: msg})
    end
  end

  defp respond(conn, _params, hex_tx, txid) do
    json(conn, %{code: 0, raw_tx: hex_tx, txid: txid})
  end

  # defp deal_with_content(params) do
  #   case params["content"] do
  #     b when is_binary(b) ->
  #       [b]

  #     m when is_map(m) ->
  #       m
  #       |> Enum.map(fn {k, v} ->
  #         i = String.to_integer(k)
  #         {i, v}
  #       end)
  #       |> Enum.sort()
  #       |> Enum.map(fn {_, v} -> v end)

  #     l when is_list(l) ->
  #       l

  #     _ ->
  #       []
  #   end
  # end

  def find(conn, %{"name" => dir}) do
    base_key = conn.assigns.private_key

    case Wallet.find_txids_with_dir(base_key, dir) do
      {:ok, txids} ->
        json(conn, %{code: 0, txids: txids})

      {:error, _} ->
        json(conn, %{code: 1, error: "mnode: #{dir}: No such file or directory"})
    end
  end

  def find(conn, %{"path" => dir} = params) do
    find(conn, Map.put(params, "name", dir))
  end

  # defp dir_type(dir) do
  #   case String.contains?(dir, "/") do
  #     true -> :noroot
  #     false -> :root
  #   end
  # end

  # defp right_base_key(b) do
  #   if b.address == "1A1QQLSnKDm5YnvSdVgxKJsKBJZw4qBKNX" do
  #     {:ok, b}
  #   else
  #     {:error, "wrong app key"}
  #   end
  # end

  # defp right_netwrok("mainnet") do
  #   case network do
  #     "mainnet" ->
  #       {:ok, "1A1QQLSnKDm5YnvSdVgxKJsKBJZw4qBKNX"}
  #     "stn" ->
  #       {:ok, "mpXMhPXm8FCLKuQ4M4fL9E5e3JAe1X6GnB"}
  #     _ ->
  #       {:error, "wrong network"}
  #   end
  # end

  # def write(conn, params) do
  #   base_key = conn.assigns.private_key

  #     case read_body(conn, length: 90_000) do
  #       {:ok, data, conn} ->
  #         # if file_size <= 90kb, use b://
  #         b_content = ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", data, type, "binary", filename]
  #         meta_content =
  #           # use simple metanet style
  #           # all child address are same
  #           ["meta", address, dir_txid]
  #         content = meta_content ++ b_content

  #     end
  #     text(conn, "ok")
  #   else
  #     text(conn, "wrong app_key")
  #   end
  # end

  # defp find_dir_txid(net, dir) do
  #   KV.get({net, dir})
  # end

  # defp continue(conn, base_key, address) do
  #   onchain_path = get_req_header(conn, "x-onchain-path")
  #   filename = Path.basename(onchain_path)
  #   type = MIME.from_path(filename)
  #   dir = Path.dirname(onchain_path)
  #   dir_txid = KV.get({:dir, address, dir})

  #   Logger.debug "onchain_path: #{onchain_path}"
  #   Logger.debug "filename: #{filename}"
  #   Logger.debug "type: #{type}"
  #   Logger.debug "dir: #{dir}"
  #   Logger.debug "address: #{address}"
  #   Logger.debug "dir_txid: #{dir_txid}"
end
