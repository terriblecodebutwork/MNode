defmodule BexWeb.IndexLive do
  @moduledoc """
  Support multi privatekey manage in different processes.
  """
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo
  alias Bex.Wallet.Utxo
  alias Bex.Wallet.PrivateKey
  alias BexWeb.Router.Helpers, as: Routes
  alias BexLib.Bitindex
  alias Bex.CoinManager
  require Logger

  def mount(_session, socket) do
    {:ok, reload(socket)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, reload(socket)}
  end

  defp reload(socket) do
    private_keys =
      Wallet.list_private_keys()
      # |> Enum.filter(fn x -> x.base_key_id == nil end)
      |> Enum.map(&Repo.preload(&1, :utxos))

    socket
    |> assign(coin_sat: CoinManager.get_coin_sat())
    |> assign(private_keys: private_keys)
    |> assign(loading: false)
  end

  def render(assigns) do
    ~L"""
    <%= if @private_keys == [] do %>
    <a href="/private_keys/new" >Import PrivateKey</a>
    <% else %>
    <ul>
      <%= for k <- @private_keys || [] do %>
        <div>
          <h2>Fund Address: <%= k.address %></h2>
          <%= if is_nil(k.base_key_id) do %>
          <button phx-click="meta" phx-value="<%= k.id %>" >Metanet</button>
          <% end %>
          <h3>UTXOs</h3>
          <%= if @loading do %>
          <h4>Loading...</h4>
          <% end %>
          <button phx-click="resync_utxo" phx-value="<%= k.id %>" >ReSync UTXOs</button>
          <div class="container">
            <ul>
              <div class="row">
                <div class="column">
                  <%= for t <- [:dust, :permission, :gold, :coin] do %>
                  <p><%= "#{t}: #{Enum.count(k.utxos, fn x -> x.type == t end)}" %></p>
                  <% end %>
                </div>
                <div class="column">
                  <h3>Value of one Coin: <%= @coin_sat %></h3>
                  <form phx-submit="set_coin_sat">
                    <input name="value" type="number">
                    <button type="submit">Set</button>
                  </form>
                </div>
              </div>
              <button phx-click="recast" phx-value="<%= k.id %>">Recast</button>
              <button phx-click="mint_all" phx-value="<%= k.id %>">Mint</button>
              <%= for u <- Enum.sort_by(k.utxos, fn u -> u.value end, &>=/2) || [] do %>
                <ul>
                    <li>type: <%= u.type %></li>
                    <li>txid: <%= u.txid %></li>
                    <li>value: <%= u.value %></li>
                </ul>
              <% end %>
            </ul>
          </div>
        </div>
      <% end %>
    </ul>
    <% end %>
    """
  end

  def handle_event("meta", id, socket) do
    {:noreply, redirect(socket, to: Routes.live_path(socket, BexWeb.MetaLive, id))}
  end

  def handle_event("set_coin_sat", %{"value" => v}, socket) do
    v = Decimal.cast(v)
    CoinManager.set_coin_sat(v)
    {:noreply, reload(socket)}
  end

  def handle_event(cmd, id, socket) do
    id = String.to_integer(id)
    send(self(), {cmd, id})
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_info({"recast", id}, socket) do
    case CoinManager.recast(id) do
      {:ok, _, hex_tx} ->
        Bitindex.broadcast_hex_tx(hex_tx)
        {:noreply, reload(socket)}

      {:error, msg} ->
        {:noreply,
         put_flash(socket, :error, msg) |> redirect(to: Routes.live_path(socket, __MODULE__))}
    end
  end

  def handle_info({"resync_utxo", id}, socket) do
    p = Repo.get!(Wallet.PrivateKey, id)
    {_, _utxos} = Wallet.sync_utxos_of_private_key(p)

    {:noreply, reload(socket)}
  end

  def handle_info({"mint_all", id}, socket) do
    {:ok, _, hex_tx} = CoinManager.mint(id)
    Bitindex.broadcast_hex_tx(hex_tx)
    {:noreply, reload(socket)}
  end
end
