defmodule BexWeb.GunLive do
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Txrepo

  @coin_sat Decimal.cast(1000)

  def mount(%{key: id, key2: id2}, socket) do
    send self(), :sync
    key = Wallet.get_private_key!(id)
    key2 = Wallet.get_private_key!(id2)
    bullet = Wallet.count_utxo(key2)
    {
      :ok,
      socket
      |> assign(:key, key)
      |> assign(:key2, key2)
      |> assign(:loading, true)
      |> assign(:spliting, false)
      |> assign(:shooting, false)
      |> assign(:balance, 0)
      |> assign(:error, "")
      |> assign(:target, "")
      |> assign(:bullet, bullet)
    }
  end

  def render(assigns) do
    ~L"""

    <h1>来复枪</h1>

    <h1><%= @error %></h1>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的 BSV: <%= @balance %> 千聪<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额 refresh</button></p>
      <p>充值浏览器: <a target="_blank" href="https://whatsonchain.com/address/<%= @key.address %>">WhatsOnChain</a></p>
      <br />
      <p>子弹 x <%= @bullet %></p>
      <button phx-click="split" <%= if @spliting, do: "disabled" %>>制造子弹 create bullet</button>
      <p>子弹浏览器: <a target="_blank" href="https://whatsonchain.com/address/<%= @key2.address %>">WhatsOnChain</a></p>
    </section>

    <section>
      <form phx-submit="gun">
        <label>目标地址:</label>
        <input value="<%= @target %>" name="target" />
        <br/>
        <button type="submit" <%= if @shooting, do: "disabled" %> >发射 shoot</button>
      </form>
    </section>

    <section>
      <h2>使用说明</h2>
      <p>请勿充值大量金额. 任何财产损失, 本网站概不负责.</p>
      <p>私钥ID保存在本地, 使用过程中请勿删除浏览器缓存.</p>
      <p>本功能不收取除矿工费之外的费用.</p>
    </section>
    """
  end

  def handle_event("gun", %{"target" => ""}, socket) do
    {:noreply, socket |> assign(:error, "") }
  end
  def handle_event("gun", %{"target" => addr}, socket) do
    key = socket.assigns.key
    key2 = socket.assigns.key2
    bullet = socket.assigns.bullet
    cond do
      addr == key.address or addr == key2.address ->
        {:noreply, assign(socket, :error, "提示: 请示用其它目标地址") }
      bullet <= 0 ->
        {:noreply, assign(socket, :error, "提示: 请先制造子弹") }
      true ->
        send self(), {:shoot, bullet}
        {:noreply, assign(socket, :target, addr) |> assign(:error, "") |> assign(:shooting, true)}
    end
  end

  def handle_event("flash", _, socket) do
    send self(), :sync
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_event("split", _, socket) do
    send self(), :split
    {:noreply, assign(socket, :spliting, true)}
  end

  def handle_info(:split, socket) do
    key = socket.assigns.key
    key2 = socket.assigns.key2
    CoinManager.mint(key.id, @coin_sat, to: key2)
    :timer.sleep(1000)
    bullet = Wallet.count_utxo(key2)
    {:noreply, assign(socket, %{bullet: bullet, spliting: false})}
  end

  def handle_info(:sync, socket) do
    key = socket.assigns.key
    Wallet.sync_utxos_of_private_key(key)
    :timer.sleep(1000)
    balance = count_coins(key)
    {:noreply, assign(socket, %{loading: false, balance: balance})}
  end

  def handle_info({:shoot, bullet}, socket) do
    key2 = socket.assigns.key2
    target = socket.assigns.target
    Txrepo.turn_on()
    if bullet > 500 do
      for _ <- 1..500 do
        CoinManager.send_to_address(key2.id, target, @coin_sat)
      end
      bullet = bullet - 500
      send self, {:shoot, bullet}
      {:noreply, assign(socket, %{bullet: bullet})}
    else
      for _ <- 1..bullet do
        CoinManager.send_to_address(key2.id, target, @coin_sat)
      end
      {:noreply, assign(socket, %{bullet: 0, shooting: false})}
    end
  end

  def handle_info({:do_send, 0, _}, socket) do
    {:noreply, socket}
  end
  def handle_info({:do_send, a, c}, socket) do
    key = socket.assigns.key
    ad_count = socket.assigns.ad_count
    balance = socket.assigns.balance

    {balance, ad_count} =
      if rem(ad_count, 9) == 0 do
        CoinManager.send_to_address(key.id, "1FUBsjgSju23wGqR47ywynyynigxvtTCyZ", @coin_sat)
        send self(), {:do_send, a-1, c}
        {balance - 1, ad_count + 1}
      else
        CoinManager.send_opreturn(key.id, [c], @coin_sat)
        send self(), {:do_send, a-1 , c}
        {balance - 1, ad_count + 1}
      end
    :timer.sleep(500)

    {:noreply, assign(socket, %{ad_count: ad_count, balance: balance})}
  end

  defp count_coins(key) do
    Wallet.count_balance(key) |> Decimal.div_int(1000) |> Decimal.to_integer()
  end
end