defmodule BexLib.Parser do
  def parse_rawtx(rawtx) do
    try do
      tx = Binary.from_hex(rawtx)

      case :sv_peer.parse('tx', tx) do
        {tx, ""} ->
          {:ok, tx}

        any ->
          {:error, any}
      end
    catch
      :error, e ->
        {:error, e}
    end
  end
end
