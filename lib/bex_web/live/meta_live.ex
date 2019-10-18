defmodule BexWeb.MetaLive do
  @moduledoc """
  create directory, upload files.
  """
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.PrivateKey
  alias Bex.CoinManager
  alias Bex.Util
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
      key: Wallet.get_private_key!(id) |> Repo.preload([:parent_key, :utxos]),
      id: id,
      derive_keys:
        PrivateKey.get_derive_keys_by_id(id) |> Enum.map(fn x -> Repo.preload(x, :parent_key) end)
    }

    {:noreply, assign(socket, state)}
  end

  defp reload(socket) do
    socket
    |> assign(
      :derive_keys,
      PrivateKey.get_derive_keys_by_id(socket.assigns.id)
      |> Enum.map(fn x -> Repo.preload(x, :parent_key) end)
    )
  end

  def render(assigns) do
    ~L"""
    <%= if @loaded do %>
    <h1>MetaNet Nodes</h1>
    <ul>
      <li>Address: <%= @key.address %></li>
      <%= for {k, v} <- Enum.group_by(@key.utxos, fn x -> x.type end) do %>
      <li><%= k %>: <%= length(v) %></li>
      <% end %>
    </ul>
    <button phx-click="resync">Resync</button>
    <form phx-submit="create_root_dir">
      <input name="dir">
      <button type="submit">Create Root Node</button>
    </form>
    <ul>
      <%= for k <- @derive_keys || [] do %>
        <div style="margin: 10px">
          <p>KeyID: <%= k.id %></p>
          <p>ParentID: <%= k.parent_key_id %></p>
          <p>Address: <%= k.address %></p>
          <p>Dir: <%= k.dir %></p>
          <p>ParentDir: <%= k.parent_key.dir %></p>
          <p>Contents: <%= Bex.MetaNode.get_node(@key, k.dir) |> inspect() %></p>

          <form phx-submit="create_sub_dir">
            <input name="dir:<%= k.id %>">
            <button type="submit">Create Child Node</button>
          </form>
        </div>
      <% end %>
    </ul>
    <% else %>
    <h1>Please import a key</h1>
    <% end %>
    """
  end

  def handle_event("create_root_dir", %{"dir" => dir}, socket) do
    %{key: key} = socket.assigns
    Logger.info("create_root_dir: #{inspect(dir)}")
    CoinManager.create_mnode(key.id, false, dir, [dir])
    {:noreply, reload(socket)}
  end

  def handle_event("resync", _, socket) do
    send self(), :resync
    {:noreply, assign(socket, :loaded, false)}
  end

  # dir is the sub-folder name
  # need connact with hold dirs
  #
  def handle_event("create_sub_dir", map, socket) do
    [{vk, dir}] = Map.to_list(map)
    "dir:" <> id = vk
    id = String.to_integer(id)
    parent_key = socket.assigns.derive_keys |> Enum.find(fn x -> x.id == id end)
    Logger.info("create_sub_dir: #{inspect({parent_key.dir, dir})}")
    dir = parent_key.dir <> "/" <> dir
    CoinManager.create_mnode(socket.assigns.key.id, parent_key.dir, dir, [dir])
    {:noreply, reload(socket)}
  end

  def handle_info(:resync, socket) do
    key = socket.assigns.key
    Wallet.sync_utxos_of_private_key(key)
    {:noreply, reload(assign(socket, :loaded, true))}
  end
end
