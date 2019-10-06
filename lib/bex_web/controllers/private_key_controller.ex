defmodule BexWeb.PrivateKeyController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.PrivateKey
  alias Bex.Repo

  def index(conn, %{"address" => address}) do
    private_key = Wallet.get_private_key_by_address!(address) |> Repo.preload(:utxos)
    render(conn, "show.html", private_key: private_key)
  end

  def index(conn, _params) do
    private_keys = Wallet.list_private_keys()
    render(conn, "index.html", private_keys: private_keys)
  end

  def new(conn, _params) do
    changeset = Wallet.change_private_key(%PrivateKey{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"private_key" => private_key_params}) do
    case Wallet.create_private_key(private_key_params) do
      {:ok, private_key} ->
        conn
        |> put_flash(:info, "Private key created successfully.")
        |> redirect(to: Routes.private_key_path(conn, :show, private_key))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    private_key = Wallet.get_private_key!(id) |> Repo.preload(:utxos)
    render(conn, "show.html", private_key: private_key)
  end

  def edit(conn, %{"id" => id}) do
    private_key = Wallet.get_private_key!(id)
    changeset = Wallet.change_private_key(private_key)
    render(conn, "edit.html", private_key: private_key, changeset: changeset)
  end

  def update(conn, %{"id" => id, "private_key" => private_key_params}) do
    private_key = Wallet.get_private_key!(id)

    case Wallet.update_private_key(private_key, private_key_params) do
      {:ok, private_key} ->
        conn
        |> put_flash(:info, "Private key updated successfully.")
        |> redirect(to: Routes.private_key_path(conn, :show, private_key))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", private_key: private_key, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    private_key = Wallet.get_private_key!(id)
    {:ok, _private_key} = Wallet.delete_private_key(private_key)

    conn
    |> put_flash(:info, "Private key deleted successfully.")
    |> redirect(to: Routes.private_key_path(conn, :index))
  end
end
