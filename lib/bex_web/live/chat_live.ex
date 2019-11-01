defmodule BexWeb.ChatLive do
  @moduledoc """
      <dl>
      <dt>费用</dt>
      <dd>每条消息花费 2 个小喇叭, 每个小喇叭价值1000聪.建议先充个 1 块钱的, 能聊 70 多条.</dd>
      <dt>安全</dt>
      <dd>私钥ID保存在本地, 使用过程中请勿删除浏览器缓存.</dd>
      <dt>帮助</dt>
      <dd>请勿充值大量金额. 任何财产损失, 本网站概不负责. 如需帮助, 请联系下方邮件.</dd>
      <dt>METANET</dt>
      <dd>在<a href="https://mom.planaria.network/#ewogICJ2IjogMywKICAicSI6IHsKICAgICJmaW5kIjogewogICAgICAiYW5jZXN0b3IudHgiOiAiZDQ0MTdhMTljZDk4ZWFlMDA5NGZhZDFhYWRjYjAwNGIwMDgxMmJlMjU0ODNkODIxMTljZjJhM2I3ZWQ0ODIyZSIKICAgIH0sCiAgICAibGltaXQiOiAxMDAKICB9Cn0=">这里</a>可以查看链上聊天的数据结构</dd>
    </dl>
  """
  use Phoenix.LiveView
  require Logger

  alias BexLib.Key
  alias Bex.Wallet
  alias Bex.CoinManager
  alias Bex.ChatEngine
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Wallet.PrivateKey

  @coin_sat Decimal.cast(1000)
  @payment_address ChatEngine.payment_address()
  @base_key_id ChatEngine.base_key_id()
  @lobby ChatEngine.root_node() <> "/大厅"
  @root_node ChatEngine.root_node()

  def mount(%{key: id}, socket) do
    send(self(), :sync)
    key = Wallet.get_private_key!(id)
    :ok = ChatEngine.subscribe()
    # FIXME maybe use another key
    base_key = Wallet.get_private_key!(@base_key_id)
    {:ok, lobby_key} = ChatEngine.key_of_dir(base_key, @lobby)
    chat_log = chat_log_under_key(lobby_key, 500)

    {
      :ok,
      socket
      |> assign(:key, key)
      |> assign(:base_key, base_key)
      |> assign(:lobby_key, lobby_key)
      |> assign(:loading, true)
      |> assign(:balance, 0)
      |> assign(:buy_laba, 0)
      |> assign(:reply_to, "大厅")
      |> assign(:content, "")
      |> assign(:size_limit, 0)
      |> assign(:chat_log, chat_log)
    }
  end

  def render(assigns) do
    ~L"""
    <div style="color: white; position: fixed; width: 100%; height: 100%; ">
      <h2 style="margin: 0;background-color: orangered; ">Little LABA ---- <%= @reply_to %></h2>
      <div style="top: 10px; width: 100%; background-color: #fff; color: black;">
        <div phx-hook="Scroll" style="position: absolute; overflow-y: scroll; height: 75%; width: 100%; left: 0; top: 30px;;">
          <%= for {_k, v} <- Enum.sort_by(@chat_log, fn {_k, v} ->
               v.time
               |> DateTime.from_naive!("Etc/UTC")
               |> DateTime.to_unix()
             end) do %>
          <div style="display: block;"><strong><%= String.slice(v.data["user"], 0, 5) %></strong>: <%= v.data["data"] %> <a target="_blank" href="https://whatsonchain.com/tx/<%= v.txid %>"><%= v.time %></a></div>
          <% end %>
        </div>
      </div>
    </div>

    <div style="height: 20%; width: 100%; bottom: 0; position: fixed; color: white; background-color: orangered;">
      <form phx-change="editing" phx-submit="laba">
        <input name="content" style="width: 70%;"></input>
        <button type="submit" style="background: white;">发送/send</button>
      </from>
      <div>
        <p>充值地址: <%= @key.address %></p>
        <p>小喇叭(LABA): <%= div(@balance, 2) %> 个<button phx-click="flash" <%= if @loading, do: "disabled" %>>刷新/refresh</button>
          <button><a href="/gun">提现/withdraw</a></button>
        <p>
      </div>
    </div>
    """
  end

  @msg_size_limit 800

  def handle_event("editing", %{"content" => c}, socket) do
    s = byte_size(c)

    size_limit =
      if s <= @msg_size_limit do
        s
      else
        "过多"
      end

    {:noreply, assign(socket, :size_limit, size_limit)}
  end

  def handle_event("laba", %{"content" => c}, socket) do
    balance = socket.assigns.balance
    size = byte_size(c)

    if size > 0 and size <= @msg_size_limit and balance >= 2 do
      send(self(), {:do_send, c})
    end

    {:noreply, assign(socket, :content, c)}
  end

  def handle_event("flash", _, socket) do
    send(self(), :sync)
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_info(:sync, socket) do
    key = socket.assigns.key
    Wallet.sync_utxos_of_private_key(key)
    :timer.sleep(1000)
    balance = count_coins(key)
    {:noreply, assign(socket, %{loading: false, balance: balance})}
  end

  def handle_info({:do_send, c}, socket) do
    key = socket.assigns.key
    base_key = socket.assigns.base_key
    balance = socket.assigns.balance

    # FIXME add more channel
    {:ok, _txid, _hex_tx} =
      CoinManager.create_mnode(
        base_key.id,
        @root_node <> "/" <> "大厅",
        UUID.uuid1(),
        ["小喇叭聊天内容", Jason.encode!(%{data: c, user: key.address})],
        change_to: @payment_address,
        fund: {key.id, 2},
        coin_sat: @coin_sat
      )

    :timer.sleep(500)

    {:noreply, assign(socket, %{balance: balance - 2, content: ""})}
  end

  def handle_info({:chat, %{msg_id: msg_id, data: data, txid: txid, time: time}}, socket) do
    chat_log = socket.assigns.chat_log
    data = List.last(data) |> Jason.decode!()

    {:noreply,
     assign(socket, :chat_log, Map.put(chat_log, msg_id, %{time: time, data: data, txid: txid}))}
  end

  def handle_info(other, socket) do
    Logger.info("unkonwn msg" <> inspect(other))
    {:noreply, socket}
  end

  defp count_coins(key) do
    CoinManager.mint(key.id, @coin_sat)
    :timer.sleep(1000)
    Wallet.count_balance(key) |> Decimal.div_int(@coin_sat) |> Decimal.to_integer()
  end

  def chat_log_under_key(key = %PrivateKey{}, n) do
    query =
      from(p in PrivateKey,
        where: p.parent_key_id == ^key.id,
        limit: ^n
      )

    children = Repo.all(query)

    Enum.reduce(children, %{}, fn x, acc ->
      case Bex.MetaNode.get_utxo_data(x.dir_txid) do
        ["小喇叭聊天内容" | data] ->
          data = List.last(data) |> Jason.decode!()
          Map.put(acc, x.dir, %{data: data, txid: x.dir_txid, time: x.inserted_at})

        _ ->
          acc
      end
    end)
  end
end
