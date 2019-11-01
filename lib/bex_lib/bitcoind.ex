defmodule BexLib.Bitcoind do
  @moduledoc """
  rpc to bitcoind.
  """
  def config do
    Application.get_env(:bex, __MODULE__)
  end

  def broadcast do
  end
end
