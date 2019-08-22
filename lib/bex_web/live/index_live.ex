defmodule BexWeb.IndexLive do
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo

  def mount(_session, socket) do
    private_keys = Wallet.list_private_keys() |> Enum.map(&Repo.preload(&1, :utxos))
    {:ok, assign(socket, private_keys: private_keys)}
  end

  def render(assigns) do
    ~L"""
    <h1>Keys</h1>
    <ul>
      <%= for k <- @private_keys || [] do %>
        <div>
          <h2><%= k.address %></h2>
          <button phx-click="get_utxo" phx-value="<%= k.address%>" >ReSync UTXOs</button>
          <h3>UTXOs</h3>
          <ul>
            <%= for u <- k.utxos || [] do %>
              <ul>
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

  def handle_event("get_utxo", address, socket) do
    IO.inspect(address)
    {:noreply, socket}
  end
end
