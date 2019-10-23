defmodule Bex.ChatEngine do
  alias Bex.CoinManager
  alias Bex.Wallet

  @topic inspect(__MODULE__)
  @root_node "小喇叭聊天室"
  @base_key_id 1
  @payment_address "19Rsk91jS1bmUASDfyKV5LbN4XCGEReoxY"

  def init_chatnode() do
    if Bex.MetaNode.get_node(@base_key_id, @root_node) == nil do
      CoinManager.create_mnode(@base_key_id, false, @root_node, ["欢迎来到小喇叭聊天室, 这里有你最好的朋友👬"],
        change_to: @payment_address
      )
    else
      IO.puts("chatnode existed.")
    end

    lobby = @root_node <> "/大厅"

    if Bex.MetaNode.get_node(@base_key_id, lobby) == nil do
      CoinManager.create_mnode(@base_key_id, @root_node, lobby, ["小喇叭聊天大厅🏟"],
        change_to: @payment_address
      )
    else
      IO.puts("lobby existed")
    end
  end

  def root_node(), do: @root_node
  def base_key_id(), do: @base_key_id
  def payment_address(), do: @payment_address

  @spec subscribe :: :ok | {:error, any}
  def subscribe do
    Phoenix.PubSub.subscribe(Bex.PubSub, @topic)
  end

  def notify(msg) do
    Phoenix.PubSub.broadcast(Bex.PubSub, @topic, {:chat, msg})
  end

  def new(key, dir, content) do
    CoinManager.create_mnode(key.id, @root_node <> "/" <> dir, UUID.uuid1(), content)
  end

  def key_of_dir(base_key, dir) do
    Wallet.find_key_with_dir(base_key, dir)
  end
end
