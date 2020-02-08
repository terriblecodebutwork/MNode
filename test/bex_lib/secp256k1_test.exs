defmodule Secp256k1Test do
  use ExUnit.Case
  alias BexLib.Key
  import BexLib.Secp256k1

  test "verify" do
    msg = "Genesis"
    priv = Key.new_private_key()
    pk = Key.private_key_to_public_key(priv)
    # sig = Secp256k1.sign(msg, priv)
    # assert true == verify(msg, sig, pk)
  end

  # test "point double and addition" do
  #   point = {
  #     Decimal.cast(0x18),
  #     Decimal.cast(0x16)
  #   }
  #   double = {
  #     Decimal.cast(409),
  #     Decimal.cast(-8271)
  #   }
  #   assert point_double(point) == double
  # end

  defp verify(msg, sig, pk) do
    :crypto.verify(:ecdsa, :sha256, {:digest, msg}, sig, [pk, :secp256k1])
  end

end