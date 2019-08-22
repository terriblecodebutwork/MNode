defmodule BexLib.Api do
  alias BexLib.Bitindex

  def get_utxos_from_api(address, :bitindex) do
    case Bitindex.get_utxos_of_address(address) do
      {:ok, utxos} ->
        {:ok,
         utxos
         |> Enum.map(fn x ->
           %{
             block_height: x["height"],
             value: Decimal.cast(x["satoshis"]),
             lock_script: Binary.from_hex(x["script"]),
             index: x["vout"],
             txid: x["txid"]
           }
         end)}

      {:error, msg} ->
        {:error, msg}
    end
  end
end
