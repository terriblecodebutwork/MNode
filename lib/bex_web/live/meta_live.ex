defmodule BexWeb.MetaLive do
  @moduledoc """
  create directory, upload files.
  """
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.PrivateKey
  # alias BexWeb.Router.Helpers, as: Routes
  require Logger

  def mount(_session, socket) do
    state = %{
      loaded: false
    }

    {:ok, assign(socket, state)}
  end

  def handle_params(%{"id" => id}, _url, socket) do
    id = String.to_integer(id)

    state = %{
      loaded: true,
      key: Wallet.get_private_key!(id),
      id: id,
      derive_keys: PrivateKey.get_derive_keys_by_id(id)
    }

    {:noreply, assign(socket, state)}
  end

  defp reload(socket) do
    socket |> assign(:derive_keys, PrivateKey.get_derive_keys_by_id(socket.assigns.id))
  end

  def render(assigns) do
    ~L"""
    <%= if @loaded do %>
    <h1>MetaNet FileSystem</h1>
    <form phx-submit="create_root_dir">
      <input name="dir">
      <button type="submit">mkdir(root)</button>
    </form>
    <ul>
      <%= for k <- @derive_keys || [] do %>
        <div>
          <h2>Address: <%= k.address %></h2>
          <h2>Dir: <%= k.dir %></h2>

          <form phx-submit="create_dir">
            <input name="dir">
            <button type="submit">mkdir</button>
          </form>
        </div>
      <% end %>
    </ul>
    <% else %>
    <h1>Please load a key</h1>
    <% end %>
    """
  end

  def handle_event("create_root_dir", %{"dir" => dir}, socket) do
    %{key: key} = socket.assigns
    Logger.info("create_root_dir: #{inspect(dir)}")
    Utxo.create_root_dir(key, dir)
    {:noreply, reload(socket)}
  end

  ## TODO
  def handle_event("create_dir", %{"dir" => dir}, socket) do
    IO.inspect(dir)
    {:noreply, socket}
  end
end
