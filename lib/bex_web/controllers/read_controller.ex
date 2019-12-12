defmodule BexWeb.ReadController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.CoinManager
  alias Bex.MetaNode
  require Logger
  import BexWeb.Plug
  alias BexLib.Crypto

  plug :find_private_key
  plug :fetch_onchain_path
  plug :fetch_onchain_secret

  def remove_prefix(["TimeSV.com" | t]), do: do_remove(t)
  def remove_prefix(other), do: other

  defp do_remove(["|" | rest]), do: rest
  defp do_remove([_h | t]), do: do_remove(t)

  defp decrypt(data, secret) do
    if secret != "" do
      Crypto.aes256_decrypt(data, secret)
    else
      data
    end
  end

  def read(conn, %{}) do
    base_key = conn.assigns.private_key
    onchain_path = conn.assigns.onchain_path
    onchain_secret = conn.assigns.onchain_secret || ""

    case MetaNode.get_node(base_key, onchain_path) |> remove_prefix() do
      nil ->
        json(conn, %{code: 1, error: "can not find this path"})

      ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", data, _type, "binary", _filename] ->
        case decrypt(data, onchain_secret) do
          :error ->
            json(conn, %{code: 1, error: "decrypt fail"})

          data when is_binary(data) ->
            conn
            |> json(%{file: Base.encode64(data)})

          _ ->
            json(conn, %{code: 1, error: "unknown error in #{inspect(__MODULE__)}"})
        end

      ["15DHFxWZJT58f9nhyGnsRBqrgwK4W6h4Up", _, _type, "binary", _filename, _ | txids] ->
        data =
          Stream.resource(
            fn -> txids end,
            fn
              [] ->
                {:halt, :ok}

              txids ->
                [txid | txids] = txids

                ["1ChDHzdd1H4wSjgGMHyndZm6qxEDGjqpJL", data] =
                  MetaNode.get_utxo_data(Binary.to_hex(txid)) |> remove_prefix()

                {[data], txids}
            end,
            fn _ -> :ok end
          )
          |> Enum.into("")
          |> decrypt(onchain_secret)

        conn
        |> json(%{file: Base.encode64(data)})
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
