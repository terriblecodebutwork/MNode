defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias BexLib.Key
  require Logger

  @secret "1b49274f7149c9472be2bb3fdc868c32"
  @address "1Z2c8YiWRXGFj3zUWapfsEEJj1Qi482jZ"
  @value Decimal.cast(10000)

  @doc """
  Verify the secret.
  """
  def mb_hook(conn, %{"secret" => @secret, "payment" => payment}) do
    {user_id, user_name, content, utxo} = parse_payment(payment)

    case content && utxo do
      false ->
        Logger.error("invalid payment: #{inspect(payment)}")
        nil
      _ ->
        BsvNews.hook_msg(%{id: payment["id"], utxo: utxo, data: %{user_name: user_name, uid: user_id, content: content}})
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
        # parent is the txid of the story or comment
        {:comment, parent, data}
      _ ->
        false
    end
  end

  # mb webhook not include the index of outputs
  defp parse_utxo({x, _index}, txid) do
    v = Decimal.cast(x["satoshis"])
    case v |> Decimal.cmp(@value) do
      :lt ->
        false
      _ ->
        # hard code index
        %{txid: txid, value: v, index: 1, lock_script: Key.address_to_pkscript(x["to"])}
    end
  end

end
