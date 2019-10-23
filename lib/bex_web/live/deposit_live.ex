# defmodule BexWeb.DepositLive do
#   @moduledoc """
#   Deposit to 1A1QQLSnKDm5YnvSdVgxKJsKBJZw4qBKNX or
#   mpXMhPXm8FCLKuQ4M4fL9E5e3JAe1X6GnB
#   """
#   use Phoenix.LiveView
#   alias BexLib.Parser
#   require Logger

#   def mount(_session, socket) do
#     {:ok, assign(socket, :result, "") |> assign(:loading, false)}
#   end

#   def handle_params(%{"rawtx" => rawtx}, _url, socket) do
#     handle_event("submit", %{"rawtx" => rawtx}, socket)
#   end

#   def handle_params(_, _url, socket) do
#     {:noreply, socket}
#   end

#   def render(assigns) do
#     ~L"""
#     <h2>复制粘贴充值交易的raw tx</h2>
#     <p>主网: 1A1QQLSnKDm5YnvSdVgxKJsKBJZw4qBKNX</p>
#     <p>STN: mpXMhPXm8FCLKuQ4M4fL9E5e3JAe1X6GnB</p>
#     <%= if @loading do %>
#     <h4>Loading...</h4>
#     <% end %>

#     <form phx-submit="submit">
#       <textarea name="rawtx"></textarea>
#       <button type="submit">确定</button>
#     </form>

#     <pre><code><%= @result %></code></pre>
#     """
#   end

#   def handle_event("submit", %{"rawtx" => r}, socket) do
#     send(self(), {:parse, r})
#     {:noreply, assign(socket, :loading, true)}
#   end

#   def handle_info({:parse, r}, socket) do
#     result =
#       case Parser.parse_rawtx(r, [:human]) do
#         {:ok, r} ->
#           Jason.encode_to_iodata!(r, pretty: true)

#         {:error, e} ->
#           inspect(e)
#       end

#     {:noreply, assign(socket, :loading, false) |> assign(:result, result)}
#   end
# end
