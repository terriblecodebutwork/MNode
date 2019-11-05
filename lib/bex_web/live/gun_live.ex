defmodule BexWeb.GunLive do
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  alias Bex.Txrepo

  @coin_sat Decimal.cast(999)

  def mount(%{key: id, key2: id2}, socket) do
    send(self(), :sync)
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
    <h1>提款机</h1>

    <h1><%= @error %></h1>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的 BSV: <%= @balance %> 千聪<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额 refresh</button></p>
      <br />
    </section>

    <section>
      <form phx-change="gun">
        <label>目标地址/target address:</label>
        <input value="<%= @target %>" name="target" />
      </form>
      <button phx-click="withdraw" <%= if @shooting, do: "disabled" %>>全部转出/withdarw all</button>
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
    {:noreply, socket |> assign(:error, "")}
  end

  def handle_event("gun", %{"target" => addr}, socket) do
    key = socket.assigns.key
    key2 = socket.assigns.key2

    cond do
      Key.is_address?(addr) == false ->
        {:noreply, assign(socket, :error, "提示: 目标地址格式不正确")}

      addr == key.address or addr == key2.address ->
        {:noreply, assign(socket, :error, "提示: 请示用其它目标地址")}

      true ->
        {:noreply, assign(socket, :target, addr) |> assign(:error, "")}
    end
  end

  def handle_event("shoot", _, socket) do
    bullet = socket.assigns.bullet
    target = socket.assigns.target

    if bullet > 0 and target !== "" do
      send(self(), {:shoot, bullet})
      {:noreply, assign(socket, :shooting, true)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("withdraw", _, socket) do
    target = socket.assigns.target

    if target !== "" do
      key = socket.assigns.key
      key2 = socket.assigns.key2
      CoinManager.sweep(key, target)
      CoinManager.sweep(key2, target)
      :timer.sleep(1000)
      {:noreply, assign(socket, :error, "提示: 转出成功, 请检查钱包")}
    else
      {:noreply, assign(socket, :error, "提示: 目标地址格式不正确")}
    end
  end

  def handle_event("flash", _, socket) do
    send(self(), :sync)
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_event("split", _, socket) do
    send(self(), :split)
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
    key2 = socket.assigns.key2
    Wallet.sync_utxos_of_private_key(key)
    Wallet.sync_utxos_of_private_key(key2)
    :timer.sleep(1000)
    balance = count_coins(key)
    bullet = Wallet.count_utxo(key2)
    {:noreply, assign(socket, %{bullet: bullet, loading: false, balance: balance})}
  end

  @clip 5

  def handle_info({:shoot, bullet}, socket) do
    key2 = socket.assigns.key2
    target = socket.assigns.target
    Txrepo.turn_on()

    bullet_use = min(bullet, @clip)
    for _ <- 1..bullet_use do
      CoinManager.send_to_address(key2.id, target, @coin_sat)
    end

    {:noreply, assign(socket, %{bullet: bullet - bullet_use, shooting: false})}
  end

  defp count_coins(key) do
    Wallet.count_balance(key) |> Decimal.div_int(1000) |> Decimal.to_integer()
  end
end
