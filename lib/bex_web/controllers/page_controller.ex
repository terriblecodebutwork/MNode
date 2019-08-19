defmodule BexWeb.PageController do
  use BexWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
