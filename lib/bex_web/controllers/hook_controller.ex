defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias Bex.CoinManager

  @secret "1b49274f7149c9472be2bb3fdc868c32"


  def mb_hook(conn, %{"secret" => secret, "payment" => payment}) do
    IO.inspect secret
    user_id = payment["userId"]
    contents =
      payment["paymentOutputs"]
      |> Enum.find(fn x -> x["type"] == "SCRIPT" end)
      |> Map.get("script")
      |> parse_parent()

    text(conn, "ok")
  end
  def mb_hook(conn, params) do
    IO.inspect params
    text(conn, "ok")
  end

  defp parse_parent(str) do
    ["0", "OP_RETURN" | contents] = String.split(str)
    IO.inspect contents
  end

end
