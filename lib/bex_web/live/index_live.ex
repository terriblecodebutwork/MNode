defmodule BexWeb.IndexLive do
  @moduledoc """
  Support multi privatekey manage in different processes.
  """
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.PrivateKey

  def mount(_session, socket) do
    {:ok, reload(socket)}
  end

  defp reload(socket) do
    private_keys = Wallet.list_private_keys() |> Enum.map(&Repo.preload(&1, :utxos))
    assign(socket, private_keys: private_keys)
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
            <%= for t <- [:dust, :permission, :gold, :coin] do %>
            <p><%= "#{t}: #{Enum.count(k.utxos, fn x -> x.type == t end)}" %></p>
            <% end %>
            <button phx-click="recast" phx-value="<%= k.id %>">Recast</button>
            <%= for u <- k.utxos || [] do %>
              <ul>
                  <li>type: <%= u.type %></li>
                  <li>txid: <%= u.txid %></li>
                  <li>value: <%= u.value %></li>
                  <!-- <li>index: <%= u.index %></li> -->
                  <!-- <li>block height: <%= u.block_height %></li> -->
                  <%= if u.type == :gold do %>
                  <button phx-click="mint" phx-value="<%= u.id %>">Mint</button>
                  <% end %>
              </ul>
            <% end %>
          </ul>
          <form phx-submit="create_dir">
            <input name="dir">
          </form>
        </div>
      <% end %>
    </ul>
    """
  end

  def handle_event("resync_utxo", id, socket) do
    id = String.to_integer(id)
    p = Repo.get!(Wallet.PrivateKey, id)
    {_, utxos} = Wallet.sync_utxos_of_private_key(p)

    {:noreply, reload(socket)}
  end

  def handle_event("mint", id, socket) do
    id = String.to_integer(id)
    {:ok, _} = Utxo.mint(Repo.get!(Utxo, id))
    {:noreply, reload(socket)}
  end

  def handle_event("recast", id, socket) do
    id = String.to_integer(id)
    {:ok, _} = Utxo.recast(Repo.get!(PrivateKey, id))
    {:noreply, reload(socket)}
  end

  ## TODO
  def handle_event("create_dir", %{"dir" => dir}, socket) do
    IO.inspect(dir)
    {:noreply, socket}
  end
end
