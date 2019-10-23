defmodule Bex.Wallet.Document do
  @doc """
  "a/b/c" -> ["a", "a/b", "a/b/c"]
  """
  def get_children_dirs(dir) do
    dir
    |> String.split(["/"])
    |> Enum.scan(fn x, acc -> Enum.join([acc, x], "/") end)
  end
end
