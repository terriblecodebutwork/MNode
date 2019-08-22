defmodule BexLib.Bitindex do
  use Tesla

  @api_key "44UFrLxSBgPxt4mibqw9m9voHps7RbgT1j92YE1K7XUKefBPLMiPXq7e5Lrmpp8NWa"

  plug Tesla.Middleware.BaseUrl, "https://api.bitindex.network/api/v2"
  plug Tesla.Middleware.Headers, [{:api_key, @api_key}]
  plug Tesla.Middleware.JSON

  @doc """
  example response:
  [
    %{
     "address" => "1PVCqdqyEWGbzyBRLXptjG3n3AzaJtrsFp",
     "amount" => 2.846e-5,
     "confirmations" => 986,
     "height" => 595528,
     "outputIndex" => 1,
     "satoshis" => 2846,
     "script" => "76a914f6a84be69db9f1146f1e2eba8b61fd2d489a756c88ac",
     "scriptPubKey" => "76a914f6a84be69db9f1146f1e2eba8b61fd2d489a756c88ac",
     "txid" => "52fcce152e7396b442e81faaa2cd09d628b6daf79d49ae75cfcf34479102450f",
     "value" => 2846,
     "vout" => 1
    }
  ]
  """
  def get_utxos_of_address(addr) do
    case get("addrs/utxos?address=#{addr}") do
      {:ok, resp} ->
        {:ok, Map.get(resp.body, "data")}

      {:error, msg} ->
        {:error, msg}
    end
  end

  def broadcast_hex_tx(tx) do
    case post("/tx/send", %{hex: tx}) do
      {:ok, resp} ->
        {:ok, Map.get(resp.body, "message")}

      {:error, msg} ->
        {:error, msg}
    end
  end
end
