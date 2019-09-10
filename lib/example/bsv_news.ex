defmodule BsvNews do
  # use GenServer

  def new_post(%{user_id: id, user_name: name, content: {:story, title, txid}}) do

  end
  def new_post(%{user_id: id, user_name: name, content: {:comment, parent, data}}) do

  end
end