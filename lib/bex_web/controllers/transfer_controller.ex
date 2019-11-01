defmodule BexWeb.TransferController do
  use BexWeb, :controller

  alias BexLib.Key
  alias Bex.CoinManager
  import BexWeb.Plug, only: [find_private_key: 2]

  plug :find_private_key
  @max_withdraw 500_000_000

  def transfer(conn, %{"amount" => v}) when v > @max_withdraw do
    json(conn, %{code: 1, error: "amount larger then #{@max_withdraw} is unsupported"})
  end

  def transfer(conn, %{"to" => addr, "amount" => v}) do
    if Key.is_address?(addr, :main) do
      if is_integer(v) and v > 546 do
        do_transfer(conn, addr, v)
      else
        json(conn, %{code: 1, error: "amount must be an integer and larger than 546!"})
      end
    else
      json(conn, %{code: 1, error: "#{addr} is not a valid mainnet address."})
    end
  end

  defp do_transfer(conn, addr, v) do
    pkid = conn.assigns.private_key.id

    case CoinManager.transfer(pkid, %{to: addr, amount: v}) do
      {:ok, txid, _} ->
        json(conn, %{code: 0, txid: txid})

      {:error, msg} ->
        json(conn, %{code: 1, error: msg})
    end
  end
end
