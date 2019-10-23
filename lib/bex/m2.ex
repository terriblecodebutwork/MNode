defmodule Bex.M2 do
  alias Bex.KV
  alias BexLib.Key

  def addr("stn") do
    "mpXMhPXm8FCLKuQ4M4fL9E5e3JAe1X6GnB"
  end

  def addr("main") do
    "1A1QQLSnKDm5YnvSdVgxKJsKBJZw4qBKNX"
  end

  def utxo_unit, do: 90500

  def import_secret(hex, net) when net in [:main, :stn] do
    bn = Binary.from_hex(hex)
    address = Key.private_key_to_address(bn, net)
    app_key = :crypto.strong_rand_bytes(32) |> BexLib.Base58Check.encode()
    KV.put({:key, address}, {bn, app_key})
  end

  def keys() do
    KV.select(
      min_key: {:key, nil},
      max_key: {:key, "z"}
    )
  end
end
