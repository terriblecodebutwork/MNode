defmodule BexWeb.GunLive do
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Txrepo

  @coin_sat Decimal.cast(999)

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
    <section>
      <a href="/ad"><h1>小喇叭</h1></a>
    </section>

    <h1>来复枪</h1>

    <h1><%= @error %></h1>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的 BSV: <%= @balance %> 千聪<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额 refresh</button></p>
      <br />
      <label>子弹 x <%= @bullet %></label>
      <button phx-click="split" <%= if @spliting, do: "disabled" %>>
        <h1>制造 make</h1>
      </button>
    </section>

    <section>
      <form phx-submit="gun">
        <label>目标地址:</label>
        <input value="<%= @target %>" name="target" />
        <button type="submit" <%= if @shooting, do: "disabled" %> >
          <h1>发射 shoot</h1>
        </button>
        <p>推荐使用打点钱包收款地址, 并打开消息提示</p>
      </form>
    </section>

    <section style="bottom: 0px; position: fixed">
      <strong>用户条款</strong>
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

  @clip 5

  def handle_info({:shoot, bullet}, socket) do
    key2 = socket.assigns.key2
    target = socket.assigns.target
    Txrepo.turn_on()
    if bullet > @clip do
      for _ <- 1..@clip do
        CoinManager.send_to_address(key2.id, target, @coin_sat)
      end
      bullet = bullet - @clip
      # :timer.send_after 1000, self, {:shoot, bullet}
      {:noreply, assign(socket, %{bullet: bullet, shooting: false})}
    else
      for _ <- 1..bullet do
        CoinManager.send_to_address(key2.id, target, @coin_sat)
      end
      {:noreply, assign(socket, %{bullet: 0, shooting: false})}
    end
  end


  defp count_coins(key) do
    Wallet.count_balance(key) |> Decimal.div_int(1000) |> Decimal.to_integer()
  end
end