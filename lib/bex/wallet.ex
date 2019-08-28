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
    bn = Base.decode16!(hex, case: :mixed)

    attrs = %{
      hex: hex,
      bn: bn,
      address: Key.private_key_to_address(bn),
      app_key: :crypto.strong_rand_bytes(32) |> Base.encode64(),
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
  Add a coin type utxo into the inputs of a mission.
  Return: {:ok, mission} or {:error, any}
  """
  def consume_a_coin(%Mission{} = mission) do
    Repo.transaction(fn ->
      coin =
        from(u in Utxo,
          where: u.type == "coin" and is_nil(u.consumer_id),
          lock: "FOR UPDATE SKIP LOCKED"
        )
        |> first(:block_height)
        |> Repo.one!()

      Repo.update!(change(coin, consumer_id: mission.id))
      mission
    end)
  end

  @doc """
  Add permission to the inputs of and outputs mission, by set consumer_id
  as the mission.
  Return: {:ok, mission} or {:error, any}
  """
  def consume_permission(%Mission{}=m, dir) do
    case Repo.transaction(fn ->
        p =
          from(u in Utxo,
            where: u.type == "permission" and is_nil(u.consumer_id),
            lock: "FOR UPDATE SKIP LOCKED"
          )
          |> first(:block_height)
          |> Repo.one!()

        Repo.update!(change(p, consumer_id: m.id))
        {m, p}
      end) do
      {:ok, {mission, p}} ->
        reuse_p =
        %Utxo{
          value: p.value,
          lock_script: p.lock_script,
          private_key_id: p.private_key_id,
        }
        {:ok, Mission.add_v_output(mission, reuse_p)}
      any ->
        any
    end
  end

  # def test_lock do
  #   1..2
  #   |> Enum.map(fn _ ->
  #       spawn(fn ->
  #         get_and_lock_one_coin()
  #         |> Map.get(:id)
  #         |> IO.inspect()
  #       end)
  #     end)
  # end

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

  alias Bex.Wallet.Mission

  @doc """
  Returns the list of missions.

  ## Examples

      iex> list_missions()
      [%Mission{}, ...]

  """
  def list_missions do
    Repo.all(Mission)
  end

  @doc """
  Gets a single mission.

  Raises `Ecto.NoResultsError` if the Mission does not exist.

  ## Examples

      iex> get_mission!(123)
      %Mission{}

      iex> get_mission!(456)
      ** (Ecto.NoResultsError)

  """
  def get_mission!(id), do: Repo.get!(Mission, id)

  @doc """
  Creates a mission.

  ## Examples

      iex> create_mission(%{field: value})
      {:ok, %Mission{}}

      iex> create_mission(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_mission(attrs \\ %{}) do
    %Mission{}
    |> Mission.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a mission.

  ## Examples

      iex> update_mission(mission, %{field: new_value})
      {:ok, %Mission{}}

      iex> update_mission(mission, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_mission(%Mission{} = mission, attrs) do
    mission
    |> Mission.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Mission.

  ## Examples

      iex> delete_mission(mission)
      {:ok, %Mission{}}

      iex> delete_mission(mission)
      {:error, %Ecto.Changeset{}}

  """
  def delete_mission(%Mission{} = mission) do
    Repo.delete(mission)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking mission changes.

  ## Examples

      iex> change_mission(mission)
      %Ecto.Changeset{source: %Mission{}}

  """
  def change_mission(%Mission{} = mission) do
    Mission.changeset(mission, %{})
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

  ## Examples

      iex> create_document(%{field: value})
      {:ok, %Document{}}

      iex> create_document(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_document(attrs \\ %{}) do
    %Document{

    }
    |> Document.changeset(attrs)
    |> Repo.insert()
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

  @doc """
  build tx and broadcast and change the mission status.
  """
  def start_mission(%Mission{}=m, :one_coin) do
    # add one coin into inputs
    # add change if have any


  end

  ####### Documents ##############


  # @spec to_mission(document()) :: {:ok, mission() | [mission()]} | {:error, any()}
  @doc """
  upload the document into the blockchain.
  """
  def upload_document(%Document{type: "directory"} = d) do
    dirs = Document.get_children_dirs(d.dir)
    case dirs do
      [] ->
        {:error, "found no dir"}
      [root] ->
        # root dir
        create_root_dir(d, root)
      other ->
        [father_dir, self_dir] = Enum.take(other, -2)
        create_noroot_dir(d, father_dir, self_dir)
    end
  end

  def create_root_dir(d, root) do
    {:ok, m} = create_mission(%{document_id: d.id})
    {:ok, m} = consume_a_coin(m)
    m = Repo.preload(m, :inputs)
    inputs = m.inputs
    opreturn_script = Script.metanet("1NudAQfwpm2jmbiKjUpBjYo42ZXMgQFRsU", "Hello photonet. This is my photo album.") |> Binary.to_hex() |> IO.inspect()

  end

  def create_noroot_dir(d, father_dir, self_dir) do
    {:ok, mission} = create_mission()
    {:ok, mission} = consume_permission(mission, father_dir)
    start_mission(mission, :one_coin)
  end


end
