defmodule BexWeb.ParserLive do
  @moduledoc """
  Parse raw tx.
  """
  use Phoenix.LiveView
  alias BexLib.Parser
  require Logger

  def mount(_session, socket) do
    {:ok, assign(socket, :tx, "") |> assign(:loading, false)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end


  def render(assigns) do
    ~L"""
    <h3>Parse raw bsv transaction</h3>
    <%= if @loading do %>
    <h4>Loading...</h4>
    <% end %>

    <form phx-submit="submit">
      <textarea name="rawtx"></textarea>
      <button type="submit">Parse</button>
    </form>

    <pre><code><%= @tx %></code></pre>

    """
  end

  def handle_event("submit", %{"rawtx" => r}, socket) do
    send self(), {:parse, r}
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_info({:parse, r}, socket) do
    tx = case Parser.parse_rawtx(r) do
      {:ok, r} ->
        Jason.encode_to_iodata!(clean(r), pretty: true)
      {:error, e} ->
        inspect(e)
    end
    {:noreply, assign(socket, :loading, false) |> assign(:tx, tx)}
  end

  defp clean(tx) do
    [new_in, new_out] =
      [tx.input, tx.output]
      |> Enum.map(fn x ->
        x
        |> Enum.map(fn x ->
            x
            |> Map.delete(:raw_script)
            |> Map.delete(:tx_ref)
            |> Map.update!(:script, fn x ->
                clean_script(x) end)
        end)
      end)
    %{tx | input: new_in, output: new_out} |> IO.inspect()
  end

  defp clean_script(script) do
    Enum.map(script, fn x ->
      case x do
        x when is_tuple(x) ->
          x
          |> Tuple.to_list()
          |> Enum.map(fn x ->
            case x do
              x when is_list(x) ->
                List.to_string(x)
              x when is_binary(x) ->
                Binary.to_hex(x)
              x ->
                x
            end
          end)
        x when is_list(x) ->
          List.to_string(x)
      end
    end)
  end
end
