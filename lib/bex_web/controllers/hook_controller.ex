defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias BexLib.Key

  @secret "1b49274f7149c9472be2bb3fdc868c32"
  @address "1Z2c8YiWRXGFj3zUWapfsEEJj1Qi482jZ"
  @value Decimal.cast(10000)

  @doc """
  Verify the secret.
  """
  def mb_hook(conn, %{"secret" => @secret, "payment" => payment}) do
    {user_id, _user_name, content, utxo} = parse_payment(payment)

    case content && utxo do
      false ->
        nil
      _ ->
        BsvNews.new_post(%{id: payment["id"], user_id: user_id, utxo: utxo})
    end

    text(conn, "ok")
  end
  def mb_hook(conn, params) do
    IO.inspect params
    text(conn, "ok")
  end

  def parse_payment(payment) do
    user_id = payment["userId"]
    user_name = payment["user"]["name"]
    txid = payment["txid"]
    content =
      payment["paymentOutputs"]
      |> Enum.find(fn x -> x["type"] == "SCRIPT" end)
      |> Map.get("script")
      |> parse_content()
    utxo =
      payment["paymentOutputs"]
      |> Enum.with_index()
      |> Enum.find(fn {x, _i} -> x["to"] == @address end)
      |> parse_utxo(txid)
    {user_id, user_name, content, utxo}
  end

  defp parse_content(str) do
    ["0", "OP_RETURN" | contents] = String.split(str)
    case Enum.map(contents, &Binary.from_hex/1) do
      ["MetaNetBsvNewsV1", title, txid] ->
        {:story, title, txid}
      ["MetaNetBsvNewsCo", parent, data] ->
        {:comment, parent, data}
      _ ->
        false
    end
  end

  defp parse_utxo({x, index}, txid) do
    v = Decimal.cast(x["satoshis"])
    case v |> Decimal.cmp(@value) do
      :lt ->
        false
      _ ->
        %{txid: txid, value: v, index: index, lock_script: Key.address_to_pkscript(x["to"])}
    end
  end

end
