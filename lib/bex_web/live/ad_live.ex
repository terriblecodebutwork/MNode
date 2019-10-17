defmodule BexWeb.AdLive do
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  import Ecto.Query
  alias Bex.Repo

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
      |> assign(:buy_laba, 0)
      |> assign(:ad_count, 0)
      |> assign(:content, "")
    }
  end

  def render(assigns) do
    ~L"""
    <h1>广告墙</h1>

    <nav>
      <a href="/chat">聊天室</a>
      <a href="/gun">来复枪</a>
    </nav>

    <section>
      <form phx-submit="laba">
        <label>广告内容:</label>
        <input value="<%= @content %>" name="content" ></input>
        <label>不超过 200 汉字</label>
        <br/>
        <label>发送次数:</label>
        <input placeholder=0 type="number" name="amount" />
        <br/>
        <button type="submit">发送</button>
      </from>
    </section>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的 BSV: <%= @balance %> 千聪<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额</button></p>
      <p>本次已发送: <%= @ad_count %></p>
    </section>

    <section>
      <h2>使用说明</h2>
      <p>可在<a target="_blank" href="https://genesis.bitdb.network/query/1FnauZ9aUH2Bex6JzdcV4eNX7oLSSEbxtN/ewogICJ2IjogMywKICAicSI6IHsKICAgICJmaW5kIjogewogICAgICAib3V0LmgyIjogImU0YjhhZGU1OGQ4ZWU0YmFiYWU2YjA5MWU1ODViMWU1OTI4Y2U1OWJiZGU2ODg5MGU3YWI4YjM3MzBlNTkxYThlNWI5YjQiCiAgICB9LAogICAgInByb2plY3QiOiB7CiAgICAgICJvdXQuczIiOiAxLAogICAgICAib3V0LnMzIjogMSwKICAgICAgIm91dC5sczMiOiAxLAogICAgICAiaW4iOiAxCiAgICB9LAogICAgImxpbWl0IjogNTAwMAogIH0sCiAgInIiOiB7CiAgICAiZiI6ICJbLltdIHwge2FkOiAoLm91dFswXS5zMyArIC5vdXRbMF0ubHMzKSwgZnJvbTogLmluWzBdLmUuYX1dIHwgW3JlZHVjZSAuW10gYXMgJHggKHt0OiB7fSwgY291bnQ6IDAsIHI6IFtdfTsgLiB8IGlmICR4ID09IC50IHRoZW4gLiB8IC5jb3VudCArPSAxIGVsc2UgLiB8IC5yICs9IFt7YWQ6IC50LmFkLCBmcm9tOiAudC5mcm9tLCBjb3VudDogLmNvdW50fV0gfCAudCA9ICR4IHwgLmNvdW50ID0gMCBlbmQgKV0gfCAuW10gfCAudC5jb3VudCA9IC5jb3VudCB8IC5yICs9IFsudF0gfCAuciB8IGRlbCguWzBdKSIKICB9Cn0=">这里</a>查看全部广告. </p>
      <p>请勿充值大量金额. 任何财产损失, 本网站概不负责.</p>
      <p>私钥ID保存在本地, 使用过程中请勿删除浏览器缓存.</p>
      <p>每条广告花费 1千聪费用, 本站收取总花费的10% (每 10 笔交易中, 有 1 笔用于支付费用)).</p>
    </section>
    """
  end

  def handle_event("laba", %{"amount" => a, "content" => c}, socket) do
    balance = socket.assigns.balance

    a =
      case Integer.parse(a) do
        {x, _} -> x
        _ -> 0
      end

    if a !== 0 and a <= balance and byte_size(c) <= 800 do
      send(self, {:do_send, a, c})
    end

    {:noreply, assign(socket, :sending, true) |> assign(:content, c)}
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

  def handle_info({:do_send, 0, _}, socket) do
    {:noreply, socket}
  end

  def handle_info({:do_send, a, c}, socket) do
    key = socket.assigns.key
    ad_count = socket.assigns.ad_count + 1
    balance = socket.assigns.balance

    {balance, ad_count} =
      if rem(ad_count, 9) == 0 do
        CoinManager.send_to_address(key.id, "1FUBsjgSju23wGqR47ywynyynigxvtTCyZ", @coin_sat)
        send(self(), {:do_send, a - 1, c})
        {balance - 1, ad_count}
      else
        CoinManager.send_opreturn(key.id, ["中华人民共和国成立70周年", c], @coin_sat)
        send(self(), {:do_send, a - 1, c})
        {balance - 1, ad_count}
      end

    :timer.sleep(500)

    {:noreply, assign(socket, %{ad_count: ad_count, balance: balance})}
  end

  defp count_coins(key) do
    CoinManager.mint(key.id, @coin_sat)
    :timer.sleep(1000)
    Wallet.count_balance(key) |> Decimal.div_int(1000) |> Decimal.to_integer()
  end
end
