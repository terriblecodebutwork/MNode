defmodule BexLib.Txmaker do
  # alias BexLib.Base58Check
  alias BexLib.Types.VarInteger
  alias BexLib.Key
  alias BexLib.Crypto
  alias Bex.Wallet.Utxo
  alias Bex.Repo
  require Logger

  @sat_per_byte Decimal.cast(1)

  @doc """
  hex raw tx -> txid(hex string)
  """
  def get_txid_from_hex_tx(hex) do
    Crypto.double_sha256(Binary.from_hex(hex)) |> Binary.reverse() |> Binary.to_hex()
  end

  defp get_fee(n_in, n_out, opreturn_size) do
    estimate_tx_fee(n_in, n_out, true, opreturn_size)
  end

  defp len(x) when is_binary(x), do: byte_size(x)
  defp len(x) when is_list(x), do: length(x)

  defp to_bytes(x, size, endian) when is_integer(x) do
    s = 8 * size

    case endian do
      :big ->
        <<x::size(s)-big>>

      :little ->
        <<x::size(s)-little>>
    end
  end

  defp int_to_varint(x) do
    VarInteger.serialize(x)
  end

  defp hex_to_bytes(hex) do
    Binary.from_hex(hex)
  end

  defp double_sha256(x) do
    x |> sha256() |> sha256()
  end

  defp sha256(x), do: :crypto.hash(:sha256, x)

  defp join(list), do: IO.iodata_to_binary(list)

  defp construct_input_block(inputs) do
    for txin <- inputs do
      join([
        txin.txid,
        txin.txindex,
        txin.script_len,
        txin.script,
        sequence()
      ])
    end
    |> join()
  end

  defp construct_output_block(outputs) do
    Enum.map(outputs, fn output ->
      script =
        case output do
          %Utxo{lock_script: s} ->
            s

          {dest, _amount} ->
            [
              0x76,
              0xA9,
              0x14,
              Key.address_to_public_key_hash(dest),
              0x88,
              0xAC
            ]
            |> join()

          ## what is "safe" type: https://blog.moneybutton.com/2019/08/02/money-button-now-supports-safe-on-chain-data/
          %{type: "safe", data: data} ->
            safe_type_pkscript(data)

          %{type: "script", script: script} ->
            script
        end

      amount =
        case output do
          %Utxo{value: v} ->
            Decimal.to_integer(v)

          %{type: "safe"} ->
            0

          %{type: "script"} ->
            0
        end

      [
        amount |> to_bytes(8, :little),
        int_to_varint(len(script)),
        script
      ]
    end)
    |> join()
  end

  defp safe_type_pkscript(data) do
    data = if is_list(data), do: data, else: [data]

    [
      0,
      106,
      for(x <- data, do: [byte_size(x) |> op_push(), x]) |> IO.inspect()
    ]
    |> join()
  end

  defp op_push(size) when size <= 75, do: size

  defp op_push(size) do
    bytes = :binary.encode_unsigned(size, :little)

    case byte_size(bytes) do
      1 -> [0x4C, bytes]
      2 -> [0x4D, bytes]
      _ -> [0x4E, Binary.pad_trailing(bytes, 4)]
    end
  end

  defp newTxIn(script, script_len, txid, txindex, amount, pkbn) do
    %{
      script: script,
      script_len: script_len,
      txid: txid,
      txindex: txindex,
      amount: amount,
      private_key: pkbn
    }
  end

  defp sequence(), do: 0xFFFFFFFF |> to_bytes(4, :little)

  @doc """
  Params: utxos of input and output
  Return: 3 types of change.
  """
  def get_change(inputs, outputs, reserve_utxos \\ []) do
    opreturn_size =
      case Enum.find(outputs, fn x -> x.type == :data end) do
        nil -> 0
        u -> u.lock_script |> byte_size()
      end

    input_count = length(inputs)
    output_count = length(outputs)
    fee_with_change = get_fee(input_count, output_count + 1, opreturn_size)

    Logger.debug("fee: #{fee_with_change}")
    sum_of_inputs = Utxo.sum_of_value(inputs)
    sum_of_outputs = Utxo.sum_of_value(outputs)

    cond do
      Decimal.cmp(sum_of_inputs, Decimal.add(fee_with_change, sum_of_outputs)) == :lt ->
        if reserve_utxos == [] do
          :insufficient
        else
          get_change(
            hd(reserve_utxos) ++ inputs,
            outputs,
            tl(reserve_utxos)
          )
        end

      true ->
        change = Decimal.sub(sum_of_inputs, Decimal.add(fee_with_change, sum_of_outputs))

        if Decimal.cmp(change, 546) == :gt do
          {:change, change, inputs, outputs}
        else
          {:no_change, inputs, outputs}
        end
    end
  end

  @doc """
  params:
    - list of utxos
    - list of outputs:
      - {address, amount(Decimal satoshis)}
      - %{type: "safe", data: binary or list of binary}
      - %{type: "script", script: binary script}
  """
  def create_p2pkh_transaction(inputs, outputs)
      when is_list(inputs) and is_list(outputs) do
    version = 0x01 |> to_bytes(4, :little)
    sequence = sequence()
    lock_time = 0x00 |> to_bytes(4, :little)
    hash_type = 0x41 |> to_bytes(4, :little)

    input_count = int_to_varint(len(inputs))
    output_count = int_to_varint(len(outputs))

    output_block = construct_output_block(outputs)

    inputs =
      for %Utxo{} = input <- inputs do
        private_key_bn = Repo.preload(input, :private_key).private_key.bn
        script = input.lock_script
        script_len = int_to_varint(len(script))
        txid = hex_to_bytes(input.txid) |> Binary.reverse()
        txindex = input.index |> to_bytes(4, :little)
        amount = input.value |> Decimal.to_integer() |> to_bytes(8, :little)

        newTxIn(script, script_len, txid, txindex, amount, private_key_bn)
      end

    hashPrevouts = double_sha256(join(for i <- inputs, do: [i.txid, i.txindex]))
    hashSequence = double_sha256(join(for _i <- inputs, do: sequence))
    hashOutputs = double_sha256(output_block)

    inputs =
      for txin <- inputs do
        private_key = txin.private_key
        public_key = Key.private_key_to_public_key(private_key)
        public_key_len = len(public_key) |> to_bytes(1, :little)

        to_be_hashed =
          join([
            version,
            hashPrevouts,
            hashSequence,
            txin.txid,
            txin.txindex,
            txin.script_len,
            txin.script,
            txin.amount,
            sequence,
            hashOutputs,
            lock_time,
            hash_type
          ])

        hashed = sha256(to_be_hashed)

        signature = Crypto.sign(private_key, hashed) <> <<0x41>>

        script_sig =
          join([
            len(signature) |> to_bytes(1, :little),
            signature,
            public_key_len,
            public_key
          ])

        %{
          txin
          | script: script_sig,
            script_len: int_to_varint(len(script_sig))
        }
      end

    join([
      version,
      input_count,
      construct_input_block(inputs),
      output_count,
      output_block,
      lock_time
    ])
  end

  @doc """
  n_out will including opreturn output for falut tolerance.
  """
  def estimate_tx_fee(n_in, n_out, compressed, op_return_size \\ 0) do
    # version
    # input count length
    # output count length
    # grand total size of op_return outputs(s) and related field(s)
    # time lock
    estimated_size =
      4 +
        n_in * if(compressed, do: 148, else: 180) +
        len(int_to_varint(n_in)) +
        n_out * 34 +
        len(int_to_varint(n_out)) +
        op_return_size +
        4

    # 体积乘以费率得到估计的手续费
    Decimal.mult(estimated_size, @sat_per_byte)
  end

  # @doc """
  # This function just for testing.
  # send all balance back to sender, just minus fee.
  # """
  # def quick_send() do
  #   priv = "1AEB4829D9E92290EF35A3812B363B0CA87DFDA2B628060648339E9452BC923A" |> Binary.from_hex()
  #   addr = "1EMHJsiXjZmffBUWevGS5mWdoacmpt8vdH"
  #   utxos = [Resource.utxos(addr) |> IO.inspect() |> Enum.max_by(fn x -> x.amount end)]

  #   outputs = [
  #     {addr, hd(utxos).amount - 230}
  #   ]

  #   create_p2pkh_transaction(priv, utxos, outputs)
  #   |> broadcast()
  # end
  def get_txid_from_binary_tx(bn) do
    Crypto.double_sha256(bn) |> Binary.reverse() |> Binary.to_hex()
  end
end
