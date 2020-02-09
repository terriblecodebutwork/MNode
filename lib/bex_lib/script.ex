# Copyright (c) 2017 bitcoin-elixir developers (see https://github.com/comboy/bitcoin-elixir/graphs/contributors )

# Copyright (c) 2017 Kacper Ciesla

# Copyright (c) 2015 Justin Lynn

#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at

#       http://www.apache.org/licenses/LICENSE-2.0

#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
defmodule BexLib.Script do
  use BexLib.Script.Opcodes
  alias BexLib.Crypto
  alias BexLib.Secp256k1

  # Value returned when the script is invalid
  # TODO replace @invalid with more specific errors
  @invalid [{:error, :parser}]

  def parse(binary) when is_binary(binary) do
    try do
      parse([], binary)
    rescue
      # Match error can occur when there's not enough bytes after pushdata instruction
      _e in MatchError -> @invalid
    end
  end

  # not a binry
  def parse(_), do: @invalid

  def parse(script, <<>>), do: script |> Enum.reverse()

  # Opcode 0x01-0x4b: The next opcode bytes is data to be pushed onto the stack
  def parse(script, <<size, bin::binary>>) when size >= 0x01 and size <= 0x4B do
    <<data::binary-size(size), bin::binary>> = bin
    [data | script] |> parse(bin)
  end

  # OP_PUSHDATA1 The next byte contains the number of bytes to be pushed onto the stack.1
  def parse(script, <<@op_pushdata1, size, bin::binary>>) do
    <<data::binary-size(size), bin::binary>> = bin
    [data, :OP_PUSHDATA1 | script] |> parse(bin)
  end

  def parse(script, <<@op_pushdata2, size::unsigned-little-integer-size(16), bin::binary>>) do
    <<data::binary-size(size), bin::binary>> = bin
    [data, :OP_PUSHDATA2 | script] |> parse(bin)
  end

  def parse(script, <<@op_pushdata4, size::unsigned-little-integer-size(32), bin::binary>>) do
    <<data::binary-size(size), bin::binary>> = bin
    [data, :OP_PUSHDATA4 | script] |> parse(bin)
  end

  # Other opcodes
  def parse(script, <<op_code, bin::binary>>) when op_code in @op_values do
    [@op_name[op_code] | script] |> parse(bin)
  end

  def parse(script, <<op_code, bin::binary>>) when not (op_code in @op_values) do
    [:OP_UNKNOWN | script] |> parse(bin)
  end

  ##
  ## Binary serialization
  ##

  def to_binary(script) when is_list(script) do
    script
    |> Enum.map(&to_binary_word/1)
    |> Enum.join()
  end

  def to_binary_word(word)
      when is_binary(word) and byte_size(word) >= 0x01 and byte_size(word) <= 0x4B,
      do: <<byte_size(word)>> <> word

  def to_binary_word(word) when is_binary(word) and byte_size(word) <= 0xFF,
    do: <<@op_pushdata1, byte_size(word)>> <> word

  def to_binary_word(word) when is_binary(word) and byte_size(word) <= 0xFFFF,
    do: <<@op_pushdata2, byte_size(word)::unsigned-little-integer-size(16)>> <> word

  def to_binary_word(word) when is_binary(word),
    do: <<@op_pushdata4, byte_size(word)::unsigned-little-integer-size(32)>> <> word

  def to_binary_word(word) when word in @op_names, do: <<@op[word]>>

  @doc """
  Metanet protocol.
  example:
  [
    :OP_RETURN,
    "meta",
    "1NudAQfwpm2jmbiKjUpBjYo42ZXMgQFRsU",
    "NULL",
    "Hello photonet. This is my photo album."
  ]
  return: binary script
  """
  def metanet(dir_addr, contents, parent_tx) do
    ([
       :OP_RETURN,
       "meta",
       dir_addr,
       parent_tx
     ] ++ contents)
    |> to_binary()
  end

  @doc """
  we use the secret to generate k and r
  """
  def rpuzzle(secret) do
    r =
      Secp256k1.sign_with_secret_for_r(<<0::256>>, "", secret)
      |> Secp256k1.get_r()
    [
      :OP_OVER,
      :OP_3,
      :OP_SPLIT,
      :OP_NIP,
      :OP_1,
      :OP_SPLIT,
      :OP_SWAP,
      :OP_SPLIT,
      :OP_DROP,
      :OP_HASH160,
      Crypto.hash160(r),
      :OP_EQUALVERIFY,
      :OP_CHECKSIG
    ] |> to_binary()
  end

  def return(contents) when is_list(contents) do
    ([
       :OP_FALSE,
       :OP_RETURN
     ] ++ contents)
    |> to_binary()
  end

  @doc """
  Print and get the human readable opreturn data
  form hex.
  """
  def view_return(str) do
    chars =
      str
      |> Binary.from_hex()
      |> Binary.to_list()
      |> Enum.map(fn x -> [x] end)

    chars |> IO.iodata_to_binary()
  end

  def parse_hex(str) do
    str
    |> Binary.from_hex()
    |> parse()
  end
end
