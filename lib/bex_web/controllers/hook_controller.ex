defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias BexLib.Key
  require Logger

  @secret "1b49274f7149c9472be2bb3fdc868c32"

  @doc """
  Verify the secret.
  """
  def mb_hook(conn, %{"secret" => @secret, "payment" => payment}) do
    {user_id, user_name, content, txid} = parse_payment(payment)

    case content && txid do
      false ->
        Logger.error("invalid payment: #{inspect(payment)}")
        nil

      _ ->
        BsvNews.hook_msg(%{
          id: payment["id"],
          txid: txid,
          data: %{user_name: user_name, uid: user_id, content: content}
        })
    end

    text(conn, "ok")
  end

  def mb_hook(conn, params) do
    Logger.warn(inspect(params))
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

    {user_id, user_name, content, txid}
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
end
