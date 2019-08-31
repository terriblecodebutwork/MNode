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

          <form phx-submit="create_sub_dir">
            <input name="dir:<%= k.id %>">
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

  # dir is the sub-folder name
  # need connact with hold dirs
  #
  def handle_event("create_sub_dir", map, socket) do
    [{vk, dir}] = Map.to_list(map)
    "dir:" <> id = vk
    id = String.to_integer(id)
    key = socket.assigns.derive_keys |> Enum.find(fn x -> x.id == id end)
    Logger.info("create_sub_dir: #{inspect({key.dir, dir})}")
    dir = key.dir <> "/" <> dir
    Utxo.create_sub_dir(key, dir)
    {:noreply, reload(socket)}
  end
end
