defmodule BexWeb.DepositController do
  use BexWeb, :controller
  require Logger
  alias Bex.M2
  alias Bex.KV

  plug :parse_tx
  plug :fetch_utxo
  plug :save_utxo

  def deposit(conn, _params) do
    text(conn, :ok)
  end

  defp parse_tx(conn, _options) do
    raw = conn.params["tx"]
    case BexLib.Parser.parse_rawtx(raw) do
      {:error, _} ->
        conn
        |> send_resp(400, "wrong tx")
        |> halt()
      {:ok, tx} ->
        conn
        |> assign(:tx, tx)
    end
  end

  defp fetch_utxo(conn, _options) do
    tx = conn.assigns.tx
    net = conn.params["net"]
    addr = M2.addr(net)
    case tx.output |> find_outputs_to_addr(addr) do
      {:error, _} ->
        conn
        |> send_resp(400, "tx without deposit output")
        |> halt()
      {:ok, utxos} ->
        conn
        |> assign(:txid, tx.txid)
        |> assign(:addr, addr)
        |> assign(:utxos, utxos)
    end
  end

  defp save_utxo(conn, _options) do
    utxos = conn.assigns.utxos
    addr = conn.assigns.addr
    txid = conn.assigns.txid
    Enum.map(utxos, fn u ->
      KV.put({:utxo, addr, txid, u.index}, true)
    end)
    conn
  end

  defp find_outputs_to_addr(outputs, addr) do
    u = Enum.reduce(outputs, [], fn x, acc ->
      if x.raw_script == BexLib.Key.address_to_pkscript(addr) do
        [x| acc]
      else
        acc
      end
    end)

    if u == [] do
      {:error, nil}
    else
      {:ok, u}
    end
  end

end
