defmodule BexWeb.UtxoController do
  use BexWeb, :controller

  alias Bex.Wallet
  alias Bex.Wallet.Utxo

  def index(conn, _params) do
    utxos = Wallet.list_utxos()
    render(conn, "index.html", utxos: utxos)
  end

  def new(conn, _params) do
    changeset = Wallet.change_utxo(%Utxo{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"utxo" => utxo_params}) do
    case Wallet.create_utxo(utxo_params) do
      {:ok, utxo} ->
        conn
        |> put_flash(:info, "Utxo created successfully.")
        |> redirect(to: Routes.utxo_path(conn, :show, utxo))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    utxo = Wallet.get_utxo!(id)
    render(conn, "show.html", utxo: utxo)
  end

  def edit(conn, %{"id" => id}) do
    utxo = Wallet.get_utxo!(id)
    changeset = Wallet.change_utxo(utxo)
    render(conn, "edit.html", utxo: utxo, changeset: changeset)
  end

  def update(conn, %{"id" => id, "utxo" => utxo_params}) do
    utxo = Wallet.get_utxo!(id)

    case Wallet.update_utxo(utxo, utxo_params) do
      {:ok, utxo} ->
        conn
        |> put_flash(:info, "Utxo updated successfully.")
        |> redirect(to: Routes.utxo_path(conn, :show, utxo))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", utxo: utxo, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    utxo = Wallet.get_utxo!(id)
    {:ok, _utxo} = Wallet.delete_utxo(utxo)

    conn
    |> put_flash(:info, "Utxo deleted successfully.")
    |> redirect(to: Routes.utxo_path(conn, :index))
  end
end
