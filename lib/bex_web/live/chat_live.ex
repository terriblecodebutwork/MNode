defmodule BexWeb.ChatLive do
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  import Ecto.Query
  alias Bex.Repo

  @coin_sat Decimal.cast(1000)
  @root_node "小喇叭聊天室"
  @payment_address "19Rsk91jS1bmUASDfyKV5LbN4XCGEReoxY"

  def create_root() do
    CoinManager.create_mnode(1, false, @root_node, ["欢迎来到小喇叭聊天室, 这里有你最好的朋友👬"], change_to: @payment_address)
  end

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
      |> assign(:reply_to, "大厅")
      |> assign(:content, "")
      |> assign(:size_limit, 0)
    }
  end

  def render(assigns) do
    ~L"""
    <h1>小喇叭聊天室</h1>

    <section>
      <p>充值地址: <%= @key.address %></p>
      <p>我的小喇叭: <%= @balance %> 个<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新余额</button></p>
    </section>

    <div class="msgs">
      <%= msg_tree() %>
    </div>

    <section>
      <form phx-change="editing" phx-submit="laba">
        <label>发送到: <%= @reply_to %></label>
        <input value="<%= @content %>" name="content" ></input>
        <label>字数: <%= @size_limit %> bytes</label>
        <br/>
        <button type="submit">发送</button>
      </from>
    </section>

    <section>
      <h2>使用说明</h2>
      <p>每条消息花费 1 个小喇叭, 每个小喇叭价值1千聪.</p>
      <p>私钥ID保存在本地, 使用过程中请勿删除浏览器缓存.</p>
      <p>请勿充值大量金额. 任何财产损失, 本网站概不负责. 如需帮助, 请联系下方邮件.</p>
    </section>
    """
  end

  @msg_size_limit 800

  defp msg_tree() do
    {:safe, "<div>施工中🚧请勿使用</div>"}
  end

  def handle_event("editing", %{"content" => c}, socket) do
    s = byte_size(c)
    size_limit = if s <= @msg_size_limit do
      s
    else
      "过多"
    end
    {:noreply, assign(socket, :size_limit, size_limit)}
  end

  def handle_event("laba", %{"amount" => a, "content" => c}, socket) do
    balance = socket.assigns.balance

    a =
      case Integer.parse(a) do
        {x, _} -> x
        _ -> 0
      end

    if a !== 0 and a <= balance and byte_size(c) <= @msg_size_limit do
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
    balance = socket.assigns.balance

    CoinManager.send_opreturn(key.id, ["中华人民共和国成立70周年", c], @coin_sat)
    send(self(), {:do_send, a - 1, c})

    :timer.sleep(500)

    {:noreply, assign(socket, %{balance: balance - 1})}
  end

  defp count_coins(key) do
    CoinManager.mint(key.id, @coin_sat)
    :timer.sleep(1000)
    Wallet.count_balance(key) |> Decimal.div_int(1000) |> Decimal.to_integer()
  end
end
