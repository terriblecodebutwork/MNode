defmodule BexWeb.WriteController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.CoinManager
  require Logger
  import BexWeb.Plug
  alias BexLib.Crypto

  plug :find_private_key
  plug :fetch_onchain_path
  plug :fetch_onchain_info
  plug :fetch_onchain_secret

  @chunk_size 90_000

  defp encrypt(data, secret) do
    if secret != "" do
      Crypto.aes256_encrypt(data, secret)
    else
      data
    end
  end

  def write(conn, %{"file" => data}) do
    base_key = conn.assigns.private_key
    onchain_path = conn.assigns.onchain_path
    onchain_secret = conn.assigns.onchain_secret || ""
    onchain_info = conn.assigns.onchain_info || ""
    filename = Path.basename(onchain_path)
    type = MIME.from_path(filename)
    dir = Path.dirname(onchain_path)

    body_return =
      case {:ok, data, conn} do
        {:ok, data, conn} ->
          case Base.decode64(data) do
            {:ok, data} when byte_size(data) <= @chunk_size ->
              # if file_size <= 90kb, use b://
              data = encrypt(data, onchain_secret)
              hash = Crypto.sha256(data)

              {[
                 "TimeSV.com",
                 onchain_info,
                 hash,
                 "|",
                 "19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut",
                 data,
                 type,
                 "binary",
                 filename
               ], conn}

            {:ok, data} ->
              # data size larger than @chunk_size
              data = encrypt(data, onchain_secret)

              txids =
                Enum.reverse(multi_bcat(data, base_key.id, []))
                |> Enum.map(&Binary.from_hex/1)

              hash = Crypto.sha256(data)

              {[
                 "TimeSV.com",
                 onchain_info,
                 hash,
                 "|",
                 "15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up",
                 " ",
                 type,
                 "binary",
                 filename,
                 " "
               ] ++ txids, conn}

            {:more, data, conn} ->
              {:ok, data, conn} = read_all_data(conn, data)
              data = encrypt(data, onchain_secret)

              txids =
                Enum.reverse(multi_bcat(data, base_key.id, []))
                |> Enum.map(&Binary.from_hex/1)

              hash = Crypto.sha256(data)

              {[
                 "TimeSV.com",
                 onchain_info,
                 hash,
                 "|",
                 "15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up",
                 " ",
                 type,
                 "binary",
                 filename,
                 " "
               ] ++ txids, conn}

            :error ->
              :error
          end
      end

    case body_return do
      :error ->
        conn |> json(%{code: 1, error: "decode64 failed"})

      {content, conn} ->
        case CoinManager.create_mnode(base_key.id, dir, onchain_path, content,
               without_permission: false
             ) do
          {:ok, txid, hex_tx} ->
            respond(conn, nil, hex_tx, txid)

          {:error, msg} ->
            json(conn, %{code: 1, error: msg})
        end
    end
  end

  defp read_all_data(conn, result) do
    case read_body(conn) do
      {:ok, data, conn} ->
        {:ok, result <> data, conn}

      {:more, data, conn} ->
        read_all_data(conn, result <> data)
    end
  end

  defp bcat_part(data, keyid) do
    Logger.info("#{byte_size(data)} data is been sending as bcat")
    content = ["TimeSV.com", "|", "1ChDHzdd1H4wSjgGMHyndZm6qxEDGjqpJL", data]
    {:ok, txid, _} = CoinManager.send_opreturn(keyid, content)
    txid
  end

  defp multi_bcat(data, keyid, result) do
    case Binary.split_at(data, @chunk_size) do
      {part, <<>>} ->
        [bcat_part(part, keyid) | result]

      {part, rest} ->
        multi_bcat(rest, keyid, [bcat_part(part, keyid) | result])
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

    case CoinManager.create_mnode(base_key.id, dir, onchain_path, [Path.basename(onchain_path)],
           without_permission: false
         ) do
      {:ok, txid, hex_tx} ->
        respond(conn, nil, hex_tx, txid)

      {:error, msg} ->
        json(conn, %{code: 1, error: msg})
    end
  end

  defp respond(conn, _params, _hex_tx, txid) do
    json(conn, %{code: 0, txid: txid})
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
