defmodule BexWeb.IndexLive do
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo

  def mount(_session, socket) do
    reload(socket)
  end

  defp reload(socket) do
    private_keys = Wallet.list_private_keys() |> Enum.map(&Repo.preload(&1, :utxos))
    {:ok, assign(socket, private_keys: private_keys)}
  end

  def render(assigns) do
    ~L"""
    <h1>Wallet</h1>
    <ul>
      <%= for k <- @private_keys || [] do %>
        <div>
          <h2>Address: <%= k.address %></h2>
          <button phx-click="resync_utxo" phx-value="<%= k.id %>" >ReSync UTXOs</button>
          <h3>UTXOs</h3>
          <ul>
            <%= for u <- k.utxos || [] do %>
              <ul>
                  <li>type: <%= u.type %></li>
                  <li>txid: <%= u.txid %></li>
                  <li>value: <%= u.value %></li>
                  <li>index: <%= u.index %></li>
                  <li>block height: <%= u.block_height %></li>
              </ul>
            <% end %>
          </ul>
        </div>
      <% end %>
    </ul>
    """
  end

  def handle_event("resync_utxo", id, socket) do
    p = Repo.get!(Wallet.PrivateKey, id)
    Wallet.sync_utxos_of_private_key(p)
    {:ok, socket} = reload(socket)
    {:noreply, socket}
  end
end
