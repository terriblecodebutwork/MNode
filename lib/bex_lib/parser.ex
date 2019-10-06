defmodule BexLib.Parser do

  def parse_rawtx(rawtx, opts) do
    cond do
      :human in opts ->
        case parse_rawtx(rawtx) do
          {:ok, tx} ->
            {
              :ok,
              clean(tx)
            }
          any ->
            any
        end
      true ->
        parse_rawtx(rawtx)
    end
  end

  def parse_rawtx(rawtx) do
    try do
      tx_bn = Binary.from_hex(rawtx)

      case :sv_peer.parse('tx', tx_bn) do
        {tx, ""} ->
          {
            :ok,
            tx
            |> put_tx_size(tx_bn)
            # |> put_tx_fee_info()
          }

        any ->
          {:error, any}
      end
    catch
      :error, e ->
        {:error, e}
    end
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
            clean_script(x)
          end)
        end)
      end)

    %{tx | input: new_in, output: new_out}
  end

  # for BexLib.Script.parse
  defp clean_script(script) do
    Enum.map(script, fn x ->
      case x do
        x when is_atom(x) ->
          Atom.to_string(x)

        x when is_binary(x) ->
          if String.valid?(x) do
            x
          else
            "0x" <> Binary.to_hex(x)
          end
      end
    end)
  end

  defp put_tx_size(tx, bn) do
    size = byte_size(bn)
    Map.put tx, :size, size
  end

end
