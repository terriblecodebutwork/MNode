defmodule Bex.Store do
  @moduledoc """
  The Store context.
  """

  import Ecto.Query, warn: false
  alias Bex.Repo

  alias Bex.Store.Merkle
  alias Bex.Store.BlockHeader

  @doc """
  Returns the list of merkle.

  ## Examples

      iex> list_merkle()
      [%Merkle{}, ...]

  """
  def list_merkle do
    Repo.all(Merkle)
  end

  def get_merkle_path(txid) do
    if block_complete?(txid) do
      path =
        Ecto.Adapters.SQL.query!(
          Repo,
          """
          WITH RECURSIVE merkle_tree AS (
                    SELECT 0 AS level, top_id, id, pair_id, at_left
                    FROM merkle
                    WHERE merkle.id = '#{txid}'
                  UNION ALL
                    SELECT level+1, n.top_id, n.id, n.pair_id, n.at_left
                    FROM merkle n
                    INNER JOIN merkle_tree p ON p.top_id = n.id
                ) SELECT id, pair_id, level, at_left
                  FROM merkle_tree
                  ORDER BY level
          """
        )
        |> Map.get(:rows)
        |> Enum.map(fn
          [root, nil, _, _] ->
            root

          [_id, pair_id, _, at_left] ->
            if at_left do
              # itself at left, its pair at right
              ["r", pair_id]
            else
              pair_id
            end
        end)

      case path do
        [_root] -> path
        list -> [txid | list]
      end
    else
      "txid not found, maybe block downloading didn't complete"
    end
  end

  defp block_complete?(txid) do
    case Repo.get(Merkle, txid) do
      nil ->
        false

      %Merkle{block_height: h} ->
        !!get_merkle_root(h)
    end
  end

  def get_merkle_root(height) do
    from(m in Merkle,
      where:
        m.block_height == ^height and
          m.root == true
    )
    |> Repo.one()
  end

  @doc """
  Gets a single merkle.

  Raises `Ecto.NoResultsError` if the Merkle does not exist.

  ## Examples

      iex> get_merkle!(123)
      %Merkle{}

      iex> get_merkle!(456)
      ** (Ecto.NoResultsError)

  """
  def get_merkle!(id), do: Repo.get!(Merkle, id)

  @doc """
  Creates a merkle.

  ## Examples

      iex> create_merkle(%{field: value})
      {:ok, %Merkle{}}

      iex> create_merkle(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_merkle(attrs \\ %{}) do
    %Merkle{}
    |> Merkle.changeset(attrs)
    |> Repo.insert(on_conflict: :nothing)
  end

  @doc """
  Updates a merkle.

  ## Examples

      iex> update_merkle(merkle, %{field: new_value})
      {:ok, %Merkle{}}

      iex> update_merkle(merkle, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_merkle(%Merkle{} = merkle, attrs) do
    merkle
    |> Merkle.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Merkle.

  ## Examples

      iex> delete_merkle(merkle)
      {:ok, %Merkle{}}

      iex> delete_merkle(merkle)
      {:error, %Ecto.Changeset{}}

  """
  def delete_merkle(%Merkle{} = merkle) do
    Repo.delete(merkle)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking merkle changes.

  ## Examples

      iex> change_merkle(merkle)
      %Ecto.Changeset{source: %Merkle{}}

  """
  def change_merkle(%Merkle{} = merkle) do
    Merkle.changeset(merkle, %{})
  end

  ## BlockHeader

  def create_block_header(attrs \\ %{}) do
    %BlockHeader{}
    |> BlockHeader.changeset(attrs)
    |> Repo.insert(on_conflict: :replace_all, conflict_target: {:constraint, :block_headers_pkey})
  end

  @doc """
  Get the height of last merkleroot saved block.
  """
  def last_block_height() do
    from(h in BlockHeader,
      select: max(h.id)
    )
    |> Repo.one()
    |> Kernel.||(-1)
  end
end
