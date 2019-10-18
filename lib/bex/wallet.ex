defmodule Bex.Wallet do
  @moduledoc """
  The Wallet context.
  """

  import Ecto.Query, warn: false
  alias Bex.Repo
  alias Bex.Wallet.PrivateKey
  alias Bex.CoinManager
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

  def get_private_key_by_address!(address) do
    Repo.one!(
      from p in PrivateKey,
        where: p.address == ^address
    )
  end

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

  def count_balance(%PrivateKey{} = key) do
    from(u in Utxo,
      where: u.private_key_id == ^key.id,
      select: sum(u.value)
    )
    |> Repo.one!()
    |> case do
      nil -> 0
      any -> any
    end
  end

  def count_utxo(%PrivateKey{} = key) do
    from(u in Utxo,
      where: u.private_key_id == ^key.id,
      select: count()
    )
    |> Repo.one!()
  end

  @doc """
  Get and save the utoxs of a private key from api.
  """
  def sync_utxos_of_private_key(%PrivateKey{} = private_key, api \\ :bitindex) do
    coin_sat = CoinManager.get_coin_sat()

    case Api.get_utxos_from_api(private_key.address, api) do
      {:ok, utxos} ->
        from(u in Utxo, where: u.private_key_id == ^private_key.id and u.type != "data")
        |> Repo.delete_all()

        Repo.insert_all(
          Utxo,
          Enum.map(utxos, fn u ->
            u
            |> Utxo.set_utxo_type(coin_sat)
            |> Map.put(:private_key_id, private_key.id)
          end),
          returning: true
        )

      # return {integer, [utxos]}

      {:error, msg} ->
        Logger.error("sync failed")
        {:error, msg}
    end
  end

  @doc """
  Save new utxo (from mb webhook)
  """
  def save_utxo(utxo) do
    Logger.debug("saving utxo #{inspect(utxo)}")
    coin_sat = CoinManager.get_coin_sat()

    key =
      from(p in PrivateKey, where: p.lock_script == ^utxo.lock_script, limit: 1) |> Repo.one!()

    Repo.insert_all(
      Utxo,
      Enum.map([utxo], fn u ->
        u
        |> Utxo.set_utxo_type(coin_sat)
        |> Map.put(:private_key_id, key.id)
      end),
      returning: true
    )
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
      where: u.type == "coin" and u.private_key_id == ^p.id,
      lock: "FOR UPDATE SKIP LOCKED",
      limit: 1,
      order_by: [asc: :id]
    )
    |> Repo.one!()
  end

  def get_coins(%PrivateKey{} = p, v, n) do
    coins = from(u in Utxo,
      where: u.value == ^v and u.private_key_id == ^p.id,
      lock: "FOR UPDATE SKIP LOCKED",
      limit: ^n,
      order_by: [asc: :id]
    )
    |> Repo.all()

    if length(coins) == n do
      coins
    else
      raise("not enough coins in :" <> p.address)
    end
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

  def derive_and_insert_key(base = %PrivateKey{}, parent = %PrivateKey{}, dir) do
    PrivateKey.derive_changeset(base, parent, dir)
    |> Repo.insert(on_conflict: :nothing, returning: true)
  end

  # FIXME currently, we just use the latest version
  # of nodes with the same "dir"
  def find_key_with_dir(_, nil), do: {:error, nil}
  def find_key_with_dir(base = %PrivateKey{}, dir) do
    query =
      from p in PrivateKey,
        where: p.base_key_id == ^base.id and p.dir == ^dir,
        order_by: [desc: :id]

    case Repo.all(query) do
      [] ->
        {:error, nil}

      # FIXME must not have duplicate privateKeys
      one ->
        {:ok, hd(one)}
    end
  end

  # this is a temperery solution for the multi-version nodes
  def find_txids_with_dir(_base, nil), do: {:error, nil}
  def find_txids_with_dir(base = %PrivateKey{}, dir) do
    query =
      from p in PrivateKey,
        where: p.base_key_id == ^base.id and p.dir == ^dir and not is_nil(p.dir_txid),
        order_by: [desc: :inserted_at]

    case Repo.all(query) do
      nil ->
        {:error, nil}

      list ->
        {:ok, Enum.map(list, fn x -> x.dir_txid end)}
    end
  end

  # @spec get_a_permission_of_dir(String.t()) :: Utxo.t()
  def get_a_permission(%PrivateKey{} = key) do
    query =
      from u in Utxo,
        where: u.private_key_id == ^key.id and u.type == "permission",
        lock: "FOR UPDATE SKIP LOCKED",
        limit: 1,
        order_by: [asc: :id]

    Repo.one!(query)
  end
end
