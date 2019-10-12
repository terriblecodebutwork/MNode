defmodule Bex.Store do
  @moduledoc """
  The Store context.
  """

  import Ecto.Query, warn: false
  alias Bex.Repo

  alias Bex.Store.Merkle

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
        Repo.all(
          from(m in Merkle,
            join:
              p in fragment(
                """
                  (WITH RECURSIVE merkle_tree AS (
                    SELECT *
                    FROM merkle
                    WHERE merkle.id = ?
                  UNION ALL
                    SELECT n.*
                    FROM merkle n
                    INNER JOIN merkle_tree p ON p.top_id = n.id
                ) SELECT * FROM merkle_tree)
                """,
                ^txid
              ),
            on: m.id == p.id,
            select: {m.pair_id, m.id}
          )
        )
        |> Enum.map(fn
          {nil, root} ->
            root

          {pair_id, _id} ->
            pair_id
        end)

      case path do
        [root] -> path
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
end
