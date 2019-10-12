defmodule Bex.Store.MerkleSaver do
  use GenServer

  @moduledoc """
  Get and Calc and Save the Merkles, check new block every interval ms.
  """
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Store.Merkle
  alias Bex.Store

  @interval 120_000

  ## HELPERS

  def get_block_page(uri) do
    get("https://api.whatsonchain.com/v1/bsv/main#{uri}")
  end

  def get_block_by_height(height) do
    get("https://api.whatsonchain.com/v1/bsv/main/block/height/#{height}")
  end

  defp get(url) do
    %{status_code: 200, body: body} = HTTPoison.get!(url)
    Jason.decode!(body)
  end

  @doc """
  Get the height of last merkleroot saved block.
  """
  def last_block_height() do
    from(m in Merkle,
      where: m.root,
      select: max(m.block_height)
    )
    |> Repo.one()
    |> Kernel.||(-1)
  end

  def get_block_txs(height) do
    info = get_block_by_height(height)

    if info["txcount"] < 1000 do
      info["tx"]
    else
      info["pages"]["uri"]
      |> Enum.map(&get_block_page/1)
      |> List.flatten()
    end
  end

  def block_merkle_tree(height) do
    txs = get_block_txs(height)
    merkle_tree(txs)
  end

  # only 1 tx
  def merkle_tree([h]) do
    [{:root, h}]
  end

  def merkle_tree(txs) do
    l1 = lowest_merkles(txs)
    tops = Enum.map(l1, fn {_, _, t} -> t end)
    [l1 | merkle_tree(tops)]
  end

  def lowest_merkles(txs) do
    Stream.chunk_every(txs, 2)
    |> Enum.map(fn
      [h1, h2] ->
        {h1, h2, concat_hash(h1, h2)}

      [h] ->
        {h, h, concat_hash(h, h)}
    end)
  end

  defp concat_hash(h1, h2) do
    BexLib.Crypto.double_sha256(little_bn(h1) <> little_bn(h2))
    |> Binary.reverse()
    |> Binary.to_hex()
  end

  def save_merkle_tree(list, height) do
    list
    |> Enum.map(fn x -> save_merkle(x, height) end)
  end

  def save_merkle({:root, hash}, height) do
    {:ok, _} =
      Store.create_merkle(%{
        block_height: height,
        id: hash,
        root: true
      })
  end

  def save_merkle(l, h) when is_list(l) do
    Enum.map(l, &save_merkle_node(&1, h))
  end

  defp save_merkle_node({h, h, top}, height) do
    {:ok, _} =
      Store.create_merkle(%{
        block_height: height,
        id: h,
        pair_id: h,
        top_id: top
      })
  end

  defp save_merkle_node({h1, h2, top}, height) do
    {:ok, _} =
      Store.create_merkle(%{
        block_height: height,
        id: h1,
        pair_id: h2,
        top_id: top
      })

    {:ok, _} =
      Store.create_merkle(%{
        block_height: height,
        id: h2,
        pair_id: h1,
        top_id: top
      })
  end

  defp little_bn(h) do
    Binary.from_hex(h) |> Binary.reverse()
  end

  ## PUBLICS

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  ## CALLBACKS

  def init(_) do
    h = last_block_height()
    # send(self(), :download)
    {:ok, %{block_height: h + 1}}
  end

  def handle_info(:download, %{block_height: h}) do
    block_merkle_tree(h)
    |> save_merkle_tree(h)
    |> IO.inspect()

    {:noreply, %{block_height: h + 1}}
  end

  def test(h) do
    block_merkle_tree(h)
    |> save_merkle_tree(h)
  end
end
