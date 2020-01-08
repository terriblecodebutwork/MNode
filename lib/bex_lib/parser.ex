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

  def parse_merkleblock(raw, opts) do
    cond do
      :human in opts ->
        case parse_merkleblock(raw) do
          {:ok, mb} ->
            {
              :ok,
              clean_merkleblock(mb)
            }

          any ->
            any
        end

      true ->
        parse_merkleblock(raw)
    end
  end

  def parse_merkleblock(raw) do
    try do
      bn = Binary.from_hex(raw)

      case :sv_peer.parse_merkle_block(bn) do
        {mb, ""} ->
          {:ok, mb}

        any ->
          {:error, any}
      end
    catch
      :error, e ->
        {:error, e}
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
        |> Enum.map(&do_clean/1)
      end)

    %{tx | input: new_in, output: new_out}
  end

  defp clean_merkleblock(mb = %{partial_merkle_tree: pmt}) do
    %{
      block_header: %{
        bits: mb.bits,
        difficulty: mb.difficulty,
        hash: mb.hex_hash,
        merkle_root: mb.merkle_root |> Binary.reverse() |> Binary.to_hex(),
        nonce: Binary.to_hex(mb.nonce),
        pow_valid: mb.pow_valid,
        previous_block: mb.prev_block |> Binary.reverse() |> Binary.to_hex(),
        target: mb.target,
        timestamp: mb.timestamp,
        version: mb.version,
        work: mb.work
      },
      partial_merkle_tree: %{
        bits: pmt.bits |> clean_pmt_bits(),
        tx_count: pmt.num_transactions,
        hashes: pmt.hashes |> Enum.map(fn x -> x |> Binary.reverse() |> Binary.to_hex() end)
      }
    }
  end

  defp clean_pmt_bits(list) do
    if List.last(list) == false do
      List.delete_at(list, -1) |> clean_pmt_bits()
    else
      list
    end
  end

  defp do_clean(x = %{coinbase: true}) do
    x
    |> Map.delete(:raw_script)
    |> Map.delete(:tx_ref)
    |> Map.update!(:script, &IO.chardata_to_string/1)
  end

  defp do_clean(x) do
    x
    |> Map.delete(:raw_script)
    |> Map.delete(:tx_ref)
    |> Map.update!(:script, fn x ->
      clean_script(x)
    end)
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
    Map.put(tx, :size, size)
  end
end
