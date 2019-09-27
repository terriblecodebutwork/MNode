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
    |> assign(showing: [])
  end

  def render(assigns) do
    ~L"""
    <%= if @private_keys == [] do %>
      <a href="/private_keys/new" >Import PrivateKey</a>
      <% else %>

      <div>
        <h3>Value of one Coin: <%= @coin_sat %></h3>
        <form phx-submit="set_coin_sat">
              <input name="value" type="number">
              <button type="submit">Set</button>
        </form>
      </div>

      <div>
        <button phx-click="resync_all">Resync all UTXOs</button>
      </div>

      <%= if @loading do %>
      <h4>Loading...</h4>
      <% end %>

      <div>
        <table border="1">
          <tr>
            <th>address</th>
            <th>metanet</th>
            <th>coin</th>
            <th>permission</th>
            <th>gold</th>
            <th>dust</th>
            <th>commands</th>
          </tr>
        <%= for k <- @private_keys || [] do %>
          <tr>
            <td><a href="https://whatsonchain.com/address/<%= k.address %>" target="_blank"><pre><%= k.address %></pre></a></td>
            <td><%= if is_nil(k.base_key_id) do %>
              <button phx-click="meta" phx-value-id="<%= k.id %>" >Metanet</button>
            <% end %></td>
            <%= for t <- [:coin, :permission, :gold, :dust] do %>
            <td><%= Enum.count(k.utxos, fn x -> x.type == t end) %></td>
            <% end %>
            <td>
              <button phx-click="resync_utxo" phx-value-id="<%= k.id %>" >ReSync UTXOs</button>
              <button phx-click="recast" phx-value-id="<%= k.id %>">Recast</button>
              <button phx-click="mint_all" phx-value-id="<%= k.id %>">Mint</button>
              <button phx-click="show_utxos" phx-value-id="<%= k.id %>">show/hide</button>
            </td>

          </tr>
          <%= if k.id in @showing do %>
            <%= for u <- Enum.sort_by(k.utxos, fn u -> u.value end, &>=/2) || [] do %>
            <tr>
              <td>type: <%= u.type %></td>
              <td>txid: <%= u.txid %></td>
              <td>value: <%= u.value %></td>
            </tr>
            <% end %>
          <% end %>

        <% end %>
        </table>
      </div>
    <% end %>
    """
  end

  def handle_event("meta", %{"id" => id}, socket) do
    {:noreply, redirect(socket, to: Routes.live_path(socket, BexWeb.MetaLive, id))}
  end

  def handle_event("set_coin_sat", %{"value" => v}, socket) do
    v = Decimal.cast(v)
    CoinManager.set_coin_sat(v)
    {:noreply, reload(socket)}
  end

  def handle_event("resync_all", _, socket) do
    send(self(), "resync_all")
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_event(cmd, %{"id" => id}, socket) do
    id = String.to_integer(id)
    send(self(), {cmd, id})
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_info("resync_all", socket) do
    list = socket.assigns.private_keys

    for x <- list do
      Wallet.sync_utxos_of_private_key(x)
    end

    {:noreply, reload(socket)}
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

  def handle_info({"show_utxos", id}, socket) do
    showing = socket.assigns.showing

    showing =
      if id in showing do
        showing -- [id]
      else
        [id | showing]
      end

    {:noreply, socket |> assign(:showing, showing) |> assign(:loading, false)}
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
