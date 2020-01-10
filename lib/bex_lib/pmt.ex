defmodule BexLib.PMT do
  @moduledoc """
  Partial Merkle Tree Lib.
  """

  @doc """
  3 ->
  1
  11
  1111
  """
  def tree(0) do
    nil
  end
  def tree(h) do
    [1, tree(h-1), tree(h-1)]
  end


  @doc """
  3 ->
  2
  """
  def get_height_from_leaves(leaves) do
    more_than_two_pow(leaves, 1, 1)
  end

  defp more_than_two_pow(x, size, height) when x <= size, do: height
  defp more_than_two_pow(x, size, height) do
    more_than_two_pow(x, size * 2, height + 1)
  end

  @doc """
  10 ->
  a tree has more than 10 leaves
  """
  def get_tree_from_leaves(n) do
    get_height_from_leaves(n) |> tree()
  end

  @doc """
  set the node which has hash as :blue
  """
  def traverse_bits(nil, bits) do
    {nil, bits}
  end
  def traverse_bits([_, left, right], [true | bits]) do
    {left, bits} = traverse_bits(left, bits)
    {right, bits} = traverse_bits(right, bits)
    {[:red, left, right], bits}
  end
  def traverse_bits([_, left, right], [false | bits]) do
    {[:blue, left, right], bits}
  end

  @doc """
  validate the traverse result.
  """
  def validate({x, []}), do: x
  def validate(_), do: {:error, "can not decode this pmt"}

  @doc """
  red_blue_tree, hashes ->
  proved_hashes
  """
  def find_proved_hashes([:red, [:red, nil, nil], [:red, nil, nil]], [h1, h2|hashes]) do
    {[h1, h2], hashes}
  end
  def find_proved_hashes([:red, [:blue, nil, nil], [:red, nil, nil]], [_h1, h2|hashes]) do
    {[h2], hashes}
  end
  def find_proved_hashes([:red, [:red, nil, nil], [:blue, nil, nil]], [h1, _h2|hashes]) do
    {[h1], hashes}
  end
  def find_proved_hashes([1, _, _], hashes) do
    {[], hashes}
  end
  def find_proved_hashes(nil, hashes) do
    {[], hashes}
  end
  def find_proved_hashes(_, []) do
    {[], []}
  end
  def find_proved_hashes([:blue, left, right], [_h|hashes]) do
    {l1, hashes} = find_proved_hashes(left, hashes)
    {l2, hashes} = find_proved_hashes(right, hashes)
    {l1 ++ l2, hashes}
  end
  def find_proved_hashes([:red, left, right], hashes) do
    {l1, hashes} = find_proved_hashes(left, hashes)
    {l2, hashes} = find_proved_hashes(right, hashes)
    {l1 ++ l2, hashes}
  end


end


9 # txs count
|> BexLib.PMT.get_tree_from_leaves()
|> BexLib.PMT.traverse_bits([true, true, true, false, true, false, true, false, false])
|> BexLib.PMT.validate()
|> BexLib.PMT.find_proved_hashes([
    "1b9a304fe5ded11beb628184301114ed403866b20da6de86a9b2816e8a70c7fa",
    "db60fb93d736894ed0b86cb92548920a3fe8310dd19b0da7ad97e48725e1e12e",
    "220ebc64e21abece964927322cba69180ed853bb187fbc6923bac7d010b9d87a",
    "ca837db5ad733692f334145b3b9fdaca53e557a71da71d5110dc6c31f09f1889",
    "dbcfb7f7721e0110ade91481e63155b9f22d11d4fa550bc36f6d15af382c39ac"
  ])
|> BexLib.PMT.validate()
|> IO.inspect()
