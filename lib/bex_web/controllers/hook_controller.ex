defmodule BexWeb.HookController do
  use BexWeb, :controller
  alias Bex.CoinManager

  @secret "1b49274f7149c9472be2bb3fdc868c32"


  def mb_hook(conn, %{"secret" => secret, "payment" => payment}) do
    user_id = payment["userId"]
    parent =
      payment["paymentOutputs"]
      |> Enum.find(fn x -> x["type"] == "SCRIPT" end)
      |> Map.get("script")
      |> parse_parent()
    IO.inspect {user_id, parent}
    text(conn, "payment")
  end
  def mb_hook(conn, params) do
    IO.inspect params
    text(conn, "ok")
  end

  defp parse_parent(str) do
    ["0", "OP_RETURN", parent, content] = String.split(str)
    {parent, content}
  end

end
