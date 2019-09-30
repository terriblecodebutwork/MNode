defmodule BexLib.Key do
  alias BexLib.Base58Check
  # alias BexLib.Script
  alias BexLib.Crypto
  alias BexLib.Script

  @address_prefix [
    public: 0,
    script: 5,
    private: 128
  ]

  def new_private_key() do
    {_, priv} = :crypto.generate_key(:ecdh, :secp256k1)
    priv
  end

  def private_key_to_public_key(priv) do
    {publickey, _priv} = :crypto.generate_key(:ecdh, :secp256k1, priv)
    compress(publickey)
  end

  def private_key_to_public_key_hash(priv) do
    priv
    |> private_key_to_public_key()
    |> Crypto.sha256()
    |> Crypto.ripemd160()
  end

  def compress(<<_prefix::size(8), x_coordinate::size(256), y_coordinate::size(256)>>) do
    prefix =
      case rem(y_coordinate, 2) do
        0 -> 0x02
        _ -> 0x03
      end

    <<prefix::size(8), x_coordinate::size(256)>>
  end

  @doc """
  Convert public key into a Bitcoin address.

  Details can be found here: https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
  """
  def public_key_to_address(pk) do
    pk
    |> Crypto.sha256()
    |> Crypto.ripemd160()
    |> Binary.prepend(@address_prefix[:public])
    |> Base58Check.encode()
  end

  def private_key_to_address(pri) do
    pri
    |> private_key_to_public_key()
    |> public_key_to_address()
  end

  def private_key_to_wif(priv) do
    # mainnet
    prefix = <<0x80>>

    suffix =
      if compressed_priv?(priv) do
        <<0x01>>
      else
        ""
      end

    (prefix <> priv <> suffix)
    |> Base58Check.encode()
  end

  def compressed_priv?(priv) do
    pub = priv |> private_key_to_public_key()
    byte_size(pub) == 33
  end

  def address_to_pkscript(addr) do
    [
      :OP_DUP,
      :OP_HASH160,
      address_to_public_key_hash(addr),
      :OP_EQUALVERIFY,
      :OP_CHECKSIG
    ]
    |> Script.to_binary()
  end

  def derive_key(privkey, data) do
    a = Crypto.double_sha256(data) |> :binary.decode_unsigned()

    privkey
    |> :binary.decode_unsigned()
    |> Kernel.+(a)
    |> :binary.encode_unsigned()
    |> Binary.take(32)
  end

  def address_to_public_key_hash(addr) do
    {:ok, <<_prefix::bytes-size(1), pubkeyhash::binary>>} = Base58Check.decode(addr)
    pubkeyhash
  end

  def private_key_to_p2pkh_script(p) do
    pkhash = private_key_to_public_key_hash(p)

    [
      0x76,
      0xA9,
      0x14,
      pkhash,
      0x88,
      0xAC
    ]
    |> IO.iodata_to_binary()
  end
end
