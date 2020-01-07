defmodule BexWeb.IndexLive do
  @moduledoc """
  Support multi privatekey manage in different processes.
  """
  use Phoenix.LiveView
  alias Bex.Wallet
  alias Bex.Repo
  alias Bex.Wallet.PrivateKey
  alias BexWeb.Router.Helpers, as: Routes
  alias BexLib.Bitindex
  alias Bex.CoinManager
  require Logger
  import Ecto.Query

  def mount(_session, socket) do
    {:ok, reload(socket)}
  end

  def handle_params(%{"address" => address}, url, socket) do
    {:noreply, socket |> assign(:url, url) |> assign(:address, address) |> reload()}
  end

  def handle_params(_, url, socket) do
    {:noreply, socket |> assign(:url, url) |> reload()}
  end

  defp reload(socket) do
    address = socket.assigns[:address]

    key =
      if address do
        from(p in PrivateKey, where: p.address == ^address)
        |> Repo.one!()
        |> Repo.preload(:utxos)
      else
        nil
      end

    socket
    |> assign(coin_sat: CoinManager.get_coin_sat())
    |> assign(private_keys: [key])
    |> assign(loading: false)
    |> assign(showing: [])
    |> assign(batch_showing: [])
  end

  def render(assigns) do
    ~L"""
    <%= if @loading do %>
    <h4>Loading...</h4>
    <% end %>
    <%= if @private_keys == [nil] do %>
      <a href="<%= @url %>/keys/new" >导入私钥</a><span></span>
      <a href="<%= @url %>/keys">全部地址</a>

      <div style="margin-top: 15px;">
        <form phx-submit="address">
          <label>输入地址</label>
          <input name="address" />
          <button type="submit">查询</button>
        </form>
      </div>

      <button phx-click="resync_all" >重新同步全部 UTXO</button>

      <% else %>

      <div>
        <table border="1">
          <tr>
            <th>地址</th>
            <th>浏览</th>
            <th>元网</th>
            <th>coin</th>
            <th>perm</th>
            <th>gold</th>
            <th>dust</th>
            <th>操作</th>
          </tr>
        <%= for k <- @private_keys || [] do %>
          <tr>
            <td><a href="<%= URI.parse(@url).path %>/keys/<%= k.id %>"><pre><%= k.address %></pre></a></td>
            <td><a href="https://whatsonchain.com/address/<%= k.address %>" target="_blank">WOC</a></td>
            <td><%= if is_nil(k.base_key_id) do %>
              <button phx-click="meta" phx-value-id="<%= k.id %>" >查看元网结构</button>
            <% end %></td>
            <%= for t <- [:coin, :permission, :gold, :dust] do %>
            <td><%= Enum.count(k.utxos, fn x -> x.type == t end) %></td>
            <% end %>
            <td>
              <button phx-click="resync_utxo" phx-value-id="<%= k.id %>" >重新同步 UTXO</button>
              <button phx-click="recast" phx-value-id="<%= k.id %>">合并零钱</button>
              <button phx-click="mint_all" phx-value-id="<%= k.id %>">拆分大额</button>
              <button phx-click="show_utxos" phx-value-id="<%= k.id %>">UTXO 详情</button>
              <button phx-click="batch_send" phx-value-id="<%= k.id %>">批量发送</button>
            </td>

          </tr>
          <%= if k.id in @showing do %>
            <%= for u <- Enum.sort_by(k.utxos, fn u -> u.value end, &>=/2) do %>
            <tr>
              <td>type: <%= u.type %></td>
              <td>txid: <%= u.txid %></td>
              <td>value: <%= u.value %></td>
              <td>
                <form phx-submit="send_utxo">
                  <input type="text" name="addr" placeholder="Send to address">
                  <input hidden name="id" value="<%= u.id %>">
                  <button submit>Send</button>
                </form>
              </td>
            </tr>
            <% end %>
          <% end %>

          <%= if k.id in @batch_showing do %>
            <tr>
              <td>
                <form phx-submit="batch_send_utxo">
                  <input type="text" name="addr" placeholder="Send to address">
                  <input type="number" name="num" placeholder="Num of utoxs">
                  <input hidden name="id" value="<%= k.id %>">
                  <button submit>Send</button>
                </form>
              </td>
            </tr>
          <% end %>

        <% end %>
        </table>
      </div>
    <% end %>
    """
  end

  def handle_event("meta", %{"id" => id}, socket) do
    {:noreply, redirect(socket, to: Routes.live_path(socket, BexWeb.MetaLive, id: id))}
  end

  def handle_event("send_utxo", %{"id" => id, "addr" => addr}, socket) do
    id = String.to_integer(id)
    CoinManager.sweep_utxo(id, addr)
    {:noreply, socket}
  end

  def handle_event("batch_send_utxo", %{"id" => id, "addr" => addr, "num" => n}, socket) do
    keys = socket.assigns.private_keys
    id = String.to_integer(id)
    {[key], keys} = Enum.split_while(keys, fn k -> k.id == id end)
    utxos = key |> Map.get(:utxos)
    Logger.info "[Batch] utxos: #{inspect utxos}"
    {u1, u2} = Enum.split(utxos, n)
    for %{id: id} <- u1 do
      Logger.info "[Batch] sweeping... utxo_id: #{inspect id}, address: #{addr}"
      CoinManager.sweep_utxo(id, addr)
    end
    key = %{key | utxos: u2}
    keys = [key | keys]
    {:noreply, socket |> assign(:private_keys, keys)}
  end

  def handle_event("address", %{"address" => addr}, socket) do
    {:noreply, redirect(socket, to: Routes.live_path(socket, BexWeb.IndexLive, address: addr))}
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
    Logger.info "cmd: #{inspect cmd} -> info"
    id = String.to_integer(id)
    send(self(), {cmd, id})
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_info("resync_all", socket) do
    list = Repo.all(PrivateKey)

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

  def handle_info({"batch_send", id}, socket) do
    batch_showing = socket.assigns.batch_showing

    batch_showing =
      if id in batch_showing do
        batch_showing -- [id]
      else
        [id | batch_showing]
      end

    {:noreply, socket |> assign(:batch_showing, batch_showing) |> assign(:loading, false)}
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
