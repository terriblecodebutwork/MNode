defmodule Bex.Wallet do
  @moduledoc """
  The Wallet context.
  """

  import Ecto.Query, warn: false
  alias Bex.Repo

  alias BexLib.Key
  alias Bex.Wallet.PrivateKey
  alias Bex.Wallet.Mission
  alias BexLib.Script
  import Ecto.Changeset, only: [change: 2]
  require Logger

  @doc """
  Returns the list of private_keys.

  ## Examples

      iex> list_private_keys()
      [%PrivateKey{}, ...]

  """
  def list_private_keys do
    Repo.all(PrivateKey)
  end

  def find_private_key_by_app_key(app_key) do
    query = from p in PrivateKey, where: p.app_key == ^app_key
    Repo.one(query)
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
    PrivateKey.hex_changeset(%{hex: hex})
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
  def sync_utxos_of_private_key(%PrivateKey{} = private_key, api \\ :bitindex) do
    case Api.get_utxos_from_api(private_key.address, api) do
      {:ok, utxos} ->
        Repo.delete_all(Ecto.assoc(private_key, :utxos))

        Repo.insert_all(
          Utxo,
          Enum.map(utxos, fn u ->
            u
            |> Utxo.set_utxo_type()
            |> Map.put(:private_key_id, private_key.id)
          end),
          returning: true
        )

      # return {integer, [utxos]}

      {:error, msg} ->
        Logger.error "sync failed"
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


  def get_a_coin(%PrivateKey{} = p) do
      from(u in Utxo,
        where: u.type == "coin",
        lock: "FOR UPDATE SKIP LOCKED",
        limit: 1
      ) |> Repo.one!()
  end


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


  alias Bex.Wallet.Document

  @doc """
  Returns the list of documents.

  ## Examples

      iex> list_documents()
      [%Document{}, ...]

  """
  def list_documents do
    Repo.all(Document)
  end

  @doc """
  Gets a single document.

  Raises `Ecto.NoResultsError` if the Document does not exist.

  ## Examples

      iex> get_document!(123)
      %Document{}

      iex> get_document!(456)
      ** (Ecto.NoResultsError)

  """
  def get_document!(id), do: Repo.get!(Document, id)

  @doc """
  Creates a document.
  """
  def create_document(attrs, base=%PrivateKey{}) do
    ## drive and set private key
    {:ok, p} = derive_and_insert_key(base, attrs.dir)

    %Document{}
    |> Document.changeset(Map.put(attrs, :private_key_id, p.id))
    |> Repo.insert()
  end

  def derive_and_insert_key(base=%PrivateKey{}, dir) do
    PrivateKey.derive_changeset(base, dir)
    |> Repo.insert(on_conflict: :nothing, returning: true)
  end

  @doc """
  Updates a document.

  ## Examples

      iex> update_document(document, %{field: new_value})
      {:ok, %Document{}}

      iex> update_document(document, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_document(%Document{} = document, attrs) do
    document
    |> Document.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Document.

  ## Examples

      iex> delete_document(document)
      {:ok, %Document{}}

      iex> delete_document(document)
      {:error, %Ecto.Changeset{}}

  """
  def delete_document(%Document{} = document) do
    Repo.delete(document)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking document changes.

  ## Examples

      iex> change_document(document)
      %Ecto.Changeset{source: %Document{}}

  """
  def change_document(%Document{} = document) do
    Document.changeset(document, %{})
  end

  # @spec get_a_permission_of_dir(String.t()) :: Utxo.t()
  def get_a_permission_of_dir(dir) do

  end


  ####### Documents ##############


  # @spec to_mission(document()) :: {:ok, mission() | [mission()]} | {:error, any()}
  @doc """
  upload the document into the blockchain.
  """
  def upload_document(%Document{type: "directory"} = d) do
    d = Repo.preload(d, :private_key) |> Repo.preload(:base_key)
    dirs = Document.get_children_dirs(d.dir)
    case dirs do
      [] ->
        {:error, "found no dir"}
      [root] ->
        # root dir
        Utxo.create_root_dir(d.base_key, d.private_key, root)
      other ->
        [father_dir, self_dir] = Enum.take(other, -2)
        # create_noroot_dir(d, father_dir, self_dir)
    end
  end



end
