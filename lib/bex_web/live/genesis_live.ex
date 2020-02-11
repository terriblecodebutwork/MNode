defmodule BexWeb.GenesisLive do
  use Phoenix.LiveView
  require Logger

  alias Bex.Wallet
  alias Bex.CoinManager

  @coin_sat Decimal.cast(1000)

  def mount(%{key: id}, socket) do
    send(self(), :sync)
    key = Wallet.get_private_key!(id)

    {
      :ok,
      socket
      |> assign(:key, key)
      |> assign(:loading, true)
      |> assign(:balance, 0)
      |> assign(:secret1, "")
      |> assign(:secret2, "")
      |> assign(:txid1, "")
      |> assign(:txid2, "")
    }
  end

  def render(assigns) do
    ~L"""
    <h1>新世纪</h1>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的小喇叭: <%= @balance %> 个<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额</button></p>
      <p>注意: 请勿转入过多金额.</p>
      <a class="text-blue-500 hover:text-blue-800" href="/mnode/gun">提现</a>
    </section>

    <section>
      <h1>锁定 R-Puzzle</h1>
      <form phx-submit="lock">
        <label>设置 R-Puzzle 密码:</label>
        <input value="<%= @secret1 %>" name="secret1" ></input>
        <label>任意字符串</label>
        <br/>
        <button type="submit">锁定</button>
      </form>
    </section>

    <p>锁定交易的 TXID: <%= @txid1 %></p>

    <section>
      <h1>解锁 R-Puzzle </h1>
      <form phx-submit="unlock">
        <label>R-Puzzle 密码:</label>
        <input value="<%= @secret2 %>" name="secret2" ></input>
        <br/>
        <button type="submit">使用任意私钥签名</button>
      </form>
    </section>

    <p>解锁交易的 TXID: <%= @txid2 %></p>

    """
  end

  def handle_event("lock", %{"secret1" => secret}, socket) do
    send self(), {:do_lock, secret}

    {:noreply, socket |> assign(:secret1, secret) |> assign(:secret2, secret)}
  end

  def handle_event("unlock", %{"secret2" => secret}, socket) do
    send self(), {:do_unlock, secret}

    {:noreply, socket |> assign(:secret2, secret)}
  end

  def handle_event("flash", _, socket) do
    send(self(), :sync)
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_event("gun", _, socket) do
    {:noreply, redirect(socket, to: "/")}
  end

  def handle_info(:sync, socket) do
    key = socket.assigns.key
    Wallet.sync_utxos_of_private_key(key)
    :timer.sleep(1000)
    balance = count_coins(key)
    {:noreply, assign(socket, %{loading: false, balance: balance})}
  end

  def handle_info({:do_lock, secret}, socket) do
    key = socket.assigns.key
    balance = socket.assigns.balance

    {:ok, txid, _hex_tx} = CoinManager.send_to_r_puzzle(key.id, secret, @coin_sat)

    {:noreply, assign(socket, %{txid1: txid, balance: balance - 1 })}
  end

  def handle_info({:do_unlock, secret}, socket) do
    key = socket.assigns.key

    {:ok, txid, _hex_tx} = CoinManager.unlock_r_puzzle(key.id, key.address, secret, @coin_sat)

    {:noreply, assign(socket, %{txid2: txid})}
  end

  defp count_coins(key) do
    CoinManager.mint(key.id, @coin_sat)
    :timer.sleep(1000)
    Wallet.count_balance(key) |> Decimal.div_int(@coin_sat) |> Decimal.to_integer()
  end
end
