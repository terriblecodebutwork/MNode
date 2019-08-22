defmodule Bex.Wallet do
  @moduledoc """
  The Wallet context.
  """

  import Ecto.Query, warn: false
  alias Bex.Repo

  alias BexLib.Key
  alias Bex.Wallet.PrivateKey

  @doc """
  Returns the list of private_keys.

  ## Examples

      iex> list_private_keys()
      [%PrivateKey{}, ...]

  """
  def list_private_keys do
    Repo.all(PrivateKey)
  end

  @doc """
  Gets a single private_key.

  Raises `Ecto.NoResultsError` if the Private key does not exist.

  ## Examples

      iex> get_private_key!(123)
      %PrivateKey{}

      iex> get_private_key!(456)
      ** (Ecto.NoResultsError)

  """
  def get_private_key!(id), do: Repo.get!(PrivateKey, id)

  @doc """
  Creates a private_key.

  ## Examples

      iex> create_private_key(%{field: value})
      {:ok, %PrivateKey{}}

      iex> create_private_key(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_private_key(%{"hex" => hex}) do
    bn = Base.decode16!(hex, case: :mixed)

    attrs = %{
      hex: hex,
      bn: bn,
      address: Key.private_key_to_address(bn),
      from: -1
    }

    %PrivateKey{}
    |> PrivateKey.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a private_key.

  ## Examples

      iex> update_private_key(private_key, %{field: new_value})
      {:ok, %PrivateKey{}}

      iex> update_private_key(private_key, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_private_key(%PrivateKey{} = private_key, attrs) do
    private_key
    |> PrivateKey.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a PrivateKey.

  ## Examples

      iex> delete_private_key(private_key)
      {:ok, %PrivateKey{}}

      iex> delete_private_key(private_key)
      {:error, %Ecto.Changeset{}}

  """
  def delete_private_key(%PrivateKey{} = private_key) do
    Repo.delete(private_key)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking private_key changes.

  ## Examples

      iex> change_private_key(private_key)
      %Ecto.Changeset{source: %PrivateKey{}}

  """
  def change_private_key(%PrivateKey{} = private_key) do
    PrivateKey.changeset(private_key, %{})
  end

  alias Bex.Wallet.Utxo
  alias BexLib.Api

  @doc """
  Returns the list of utxos.

  ## Examples

      iex> list_utxos()
      [%Utxo{}, ...]

  """
  def list_utxos do
    Repo.all(Utxo)
  end

  @doc """
  Get and save the utoxs of a private key from api.
  """
  def sync_utxos_of_private_key(private_key, api \\ :bitindex) do
    case Api.get_utxos_from_api(private_key.address, api) do
      {:ok, utxos} ->
        Repo.delete_all(Ecto.assoc(private_key, :utxos))

        Repo.insert_all(
          Utxo,
          Enum.map(utxos, fn u ->
            u
            |> Utxo.set_utxo_type()
            |> Map.put(:private_key_id, private_key.id)
          end)
        )

      {:error, msg} ->
        {:error, msg}
    end
  end

  @doc """
  Split a gold utxo into many coin utxo.
    1. send tx
    2. update db
  """
  def mint(gold = %Utxo{type: :gold}) do
    {:ok, coins} =
      gold
      |> Utxo.mint()

    # TODO add lock

    Repo.transaction(fn ->
      Repo.insert_all(Utxo, coins)
      Repo.delete(gold)
    end)
  end

  @doc """
  Gets a single utxo.

  Raises `Ecto.NoResultsError` if the Utxo does not exist.

  ## Examples

      iex> get_utxo!(123)
      %Utxo{}

      iex> get_utxo!(456)
      ** (Ecto.NoResultsError)

  """
  def get_utxo!(id), do: Repo.get!(Utxo, id)

  @doc """
  Creates a utxo.

  ## Examples

      iex> create_utxo(%{field: value})
      {:ok, %Utxo{}}

      iex> create_utxo(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_utxo(attrs \\ %{}) do
    %Utxo{}
    |> Utxo.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a utxo.

  ## Examples

      iex> update_utxo(utxo, %{field: new_value})
      {:ok, %Utxo{}}

      iex> update_utxo(utxo, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_utxo(%Utxo{} = utxo, attrs) do
    utxo
    |> Utxo.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Utxo.

  ## Examples

      iex> delete_utxo(utxo)
      {:ok, %Utxo{}}

      iex> delete_utxo(utxo)
      {:error, %Ecto.Changeset{}}

  """
  def delete_utxo(%Utxo{} = utxo) do
    Repo.delete(utxo)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking utxo changes.

  ## Examples

      iex> change_utxo(utxo)
      %Ecto.Changeset{source: %Utxo{}}

  """
  def change_utxo(%Utxo{} = utxo) do
    Utxo.changeset(utxo, %{})
  end
end
