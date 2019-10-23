# defmodule BexWeb.DepositController do
#   use BexWeb, :controller
#   require Logger
#   alias Bex.M2
#   alias Bex.KV

#   plug :parse_tx
#   plug :fetch_utxo
#   plug :save_utxo
#   plug :split_utxo

#   def deposit(conn, _params) do
#     text(conn, :ok)
#   end

#   defp parse_tx(conn, _options) do
#     raw = conn.params["tx"]
#     case BexLib.Parser.parse_rawtx(raw) do
#       {:error, _} ->
#         conn
#         |> send_resp(400, "wrong tx")
#         |> halt()
#       {:ok, tx} ->
#         conn
#         |> assign(:tx, tx)
#     end
#   end

#   defp fetch_utxo(conn, _options) do
#     tx = conn.assigns.tx
#     net = conn.params["net"]
#     addr = M2.addr(net)
#     case tx.output |> find_outputs_to_addr(addr) do
#       {:error, _} ->
#         conn
#         |> send_resp(400, "tx without deposit output")
#         |> halt()
#       {:ok, utxos} ->
#         conn
#         |> assign(:txid, tx.txid)
#         |> assign(:addr, addr)
#         |> assign(:utxos, utxos)
#     end
#   end

#   defp save_utxo(conn, _options) do
#     utxos = conn.assigns.utxos
#     addr = conn.assigns.addr
#     txid = conn.assigns.txid
#     Enum.map(utxos, fn u ->
#       KV.put({:utxo, addr, txid, u.index}, u.value)
#     end)
#     conn
#   end

#   defp find_outputs_to_addr(outputs, addr) do
#     u = Enum.reduce(outputs, [], fn x, acc ->
#       if x.raw_script == BexLib.Key.address_to_pkscript(addr) do
#         [x| acc]
#       else
#         acc
#       end
#     end)

#     if u == [] do
#       {:error, nil}
#     else
#       {:ok, u}
#     end
#   end

#   defp split_utxo(conn, _options) do
#     addr = conn.assigns.addr
#     txid = conn.assigns.txid
#     case KV.select(
#       # {:utxo, addr, txid, index} => value
#       min_key: {:utxo, addr, txid, nil},
#       max_key: {:utxo, addr, txid, "z"}
#     ) do
#       {:error, msg} ->
#         conn
#         |> send_resp(400, inspect(msg))
#         |> halt()
#       {:ok, utxos} ->
#         # FIXME multi utxos in one tx to split
#         results =
#           for {k, v} <- utxos do
#             if v > M2.utxo_unit() do
#               do_split(k, v)
#             else
#               {:ok, nil}
#             end
#           end
#         if Enum.all?(results,
#           fn
#             {:ok, _txid} ->
#               true
#             {:error, _msg} ->
#               false
#           end) do
#           conn
#           |> json(%{txids: Enum.map(results, fn {:ok, txid} -> txid end)})
#           |> halt()
#         else
#           conn
#           |> send_resp(400, "some thing wrong: #{inspect(results)}")
#           |> halt()
#         end
#     end
#   end

#   defp do_split({:utxo, addr, txid, index}, value) do
#     unit = M2.utxo_unit()
#     keys = M2.keys()
#     case Enum.find(keys,
#       fn
#         {{:key, ^addr}, _} ->
#           true
#         _ ->
#           false
#       end) do
#       nil ->
#         {:error, nil}
#       {bn, _} ->
#         M2.make(bn, )
#     end
#   end

# end
