defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias Bex.CoinManager

  @secret "1b49274f7149c9472be2bb3fdc868c32"

  @doc """
  Verify the secret.
  """
  def mb_hook(conn, %{"secret" => @secret, "payment" => payment}) do
    user_id = payment["userId"]
    user_name = payment["user"]["name"]
    content =
      payment["paymentOutputs"]
      |> Enum.find(fn x -> x["type"] == "SCRIPT" end)
      |> Map.get("script")
      |> parse_content()
    case content do
      :invalid ->
        nil
      other ->
        BsvNews.new_post(%{user_id: user_id, user_name: user_name, content: content})
    end

    text(conn, "ok")
  end
  def mb_hook(conn, params) do
    IO.inspect params
    text(conn, "ok")
  end

  defp parse_content(str) do
    ["0", "OP_RETURN" | contents] = String.split(str)
    case Enum.map(contents, &Binary.from_hex/1) do
      ["MetaNetBsvNewsV1", title, txid] ->
        {:story, title, txid}
      ["MetaNetBsvNewsCo", parent, data] ->
        {:comment, parent, data}
      _ ->
        :invalid
    end
  end

end
