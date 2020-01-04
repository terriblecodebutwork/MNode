defmodule BexWeb.GenesisLive do
  @moduledoc """
  Genesis Update
  """
  use Phoenix.LiveView
  require Logger

  def mount(_session, socket) do
    {:ok, assign(socket, :tx, "") |> assign(:loading, false)}
  end


  def render(assigns) do
    ~L"""
    <h1>Comming Soon</h1>
    """
  end

end
