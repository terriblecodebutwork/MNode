defmodule BexWeb.ReadController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.CoinManager
  alias Bex.MetaNode
  require Logger
  import BexWeb.Plug

  plug :find_private_key
  plug :fetch_onchain_path

  def read(conn, %{}) do
    base_key = conn.assigns.private_key
    onchain_path = conn.assigns.onchain_path

    case MetaNode.get_node(base_key, onchain_path) do
      nil ->
        json(conn, %{code: 1, error: "can not find this path"})
      ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", data, _type, "binary", _filename] ->
        conn
        |> put_resp_content_type("application/octet-stream", nil)
        |> send_resp(200, data)

      ["15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up", _, _type, "binary", _filename, _ | txids] ->
        Stream.resource(
          fn -> txids end,
          fn
            [] ->
              {:halt, :ok}

            txids ->
              [txid | txids] = txids

              ["1ChDHzdd1H4wSjgGMHyndZm6qxEDGjqpJL", data] =
                MetaNode.get_utxo_data(Binary.to_hex(txid))

              {[data], txids}
          end,
          fn _ -> :ok end
        )
        |> Enum.into(
          conn
          |> put_resp_content_type("application/octet-stream", nil)
          |> send_chunked(200)
        )
    end

    # conn =
    #   conn
    #   |> send_chunked(200)

    # Enum.reduce_while(contents, conn, fn (chunk, conn) ->
    #   case Plug.Conn.chunk(conn, chunk) do
    #     {:ok, conn} ->
    #       {:cont, conn}
    #     {:error, :closed} ->
    #       {:halt, conn}
    #   end
    # end)

    # content =
    #   case read_body(conn, @read_option) do
    #     {:ok, data, _conn} ->
    #       # if file_size <= 90kb, use b://
    #       ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", data, type, "binary", filename]

    #     {:more, data, conn} ->
    #       txids = read_remain_part(conn, [bcat_part(data, base_key.id)], base_key.id) |> Enum.map(&Binary.from_hex/1)
    #       ["15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up", " ", type, "binary", filename, " "] ++ txids
    #   end

    # case CoinManager.create_mnode(base_key.id, dir, onchain_path, content) do
    #   {:ok, txid, hex_tx} ->
    #     respond(conn, nil, hex_tx, txid)

    #   {:error, msg} ->
    #     json(conn, %{code: 1, error: msg})
    # end
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
end
