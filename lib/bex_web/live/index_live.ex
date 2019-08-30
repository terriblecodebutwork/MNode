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
  require Logger

  def mount(_session, socket) do
    {:ok, reload(socket)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, reload(socket)}
  end

  defp reload(socket) do
    private_keys = Wallet.list_private_keys() |> Enum.map(&Repo.preload(&1, :utxos))
    assign(socket, private_keys: private_keys)
  end

  def render(assigns) do
    ~L"""
    <ul>
      <%= for k <- @private_keys || [] do %>
        <div>
          <h2>Address: <%= k.address %></h2>
          <button phx-click="meta" phx-value="<%= k.id %>" >Metanet</button>
          <h3>UTXOs</h3>
          <button phx-click="resync_utxo" phx-value="<%= k.id %>" >ReSync UTXOs</button>
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
        </div>
      <% end %>
    </ul>
    """
  end

  def handle_event("resync_utxo", id, socket) do
    Logger.info "resync_utxo"
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

  def handle_event("meta", id, socket) do
    {:noreply, redirect(socket, to: Routes.live_path(socket, BexWeb.MetaLive, id))}
  end

  def handle_event("recast", id, socket) do
    id = String.to_integer(id)
    case Utxo.recast(Repo.get!(PrivateKey, id)) do
      {:ok, _} ->
        {:noreply, reload(socket)}
      {:error, msg} ->
        {:noreply, put_flash(socket, :error, msg) |> redirect(to: Routes.live_path(socket, __MODULE__))}
    end
  end

end
