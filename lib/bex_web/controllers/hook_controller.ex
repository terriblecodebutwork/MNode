defmodule BexWeb.HookController do
  use BexWeb, :controller

  @secret "1b49274f7149c9472be2bb3fdc868c32"


  def mb_hook(conn, %{"secret" => secret, "payment" => payment}) do
    IO.inspect payment
    text(conn, "payment")
  end
  def mb_hook(conn, params) do
    IO.inspect params
    text(conn, "ok")
  end
end
