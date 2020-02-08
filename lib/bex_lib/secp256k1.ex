defmodule BexLib.Secp256k1 do
  @moduledoc """
  ECDSA Secp256k1 curve operations.

  By default erlang's :crypto.verify is used to make it less problematic when using
  as a library (no need for gcc when you just want to parse something).

  However, if :libsecp256k1 NIF is available, it's used. To enable it just uncomment
  appropriate line in mix.exs deps.

  libsecp256k1: https://github.com/bitcoin-core/secp256k1

  If gcc and git dependencies are not a problem, use NIF. It's much faster and it's
  the proper way to do it consensus-wise. Do note that even though it's unlikely, an error
  in the NIF or libsecp256k1 will bring the whole erlang VM down (not just the process)
  """

  alias BexLib.DERSig
  # alias BexLib.Crypto
  # import Decimal, only: [mult: 2, sub: 2, div_int: 2, add: 2]

  require Logger

  @using_nif Code.ensure_loaded?(:libsecp256k1)

  @doc """
  Verify signed message.

  Returns `true` if the signature matches public key.

  `msg` should already be a 32 byte long digest.
  """
  @spec verify(binary, binary, binary) :: boolean
  def verify(msg, sig, pk) do
    do_verify(msg, DERSig.normalize(sig), pk)
  end

  @params %{
    p: 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F,
    a: 0x00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
    b: 0x00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000007,
    G:
      0x04_79BE667E_F9DCBBAC_55A06295_CE870B07_029BFCDB_2DCE28D9_59F2815B_16F81798_483ADA77_26A3C465_5DA4FBFC_0E1108A8_FD17B448_A6855419_9C47D08F_FB10D4B8,
    n: 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_BAAEDCE6_AF48A03B_BFD25E8C_D0364141,
    h: 0x01
  }

  @doc """
  Secp256k1 parameters.

  http://www.secg.org/sec2-v2.pdf
  """
  @spec params :: map
  def params do
    @params
  end

  if @using_nif do
    Logger.info("Using libsecp256k1 NIF for ECDSA operations.")

    @spec do_verify(binary, binary, binary) :: boolean
    defp do_verify(msg, sig, pk), do: :libsecp256k1.ecdsa_verify(msg, sig, pk) == :ok
  else
    Logger.info("Using erlang implementation for ECDSA operations.")

    @spec do_verify(binary, binary, binary) :: boolean
    defp do_verify(msg, sig, pk),
      do: :crypto.verify(:ecdsa, :sha256, {:digest, msg}, sig, [pk, :secp256k1])
  end

  # def sign(msg, priv) do
  #   #1 hash
  #   e = Crypto.sha256(msg)
  #   # 2 z maybe greater than n, but not longer
  #   ln = params().n |> bit_size()
  #   <<z::size(ln), _>> = e
  #   # 3
  #   k = 1
  #   # 4

  # end

  # def point_add({xp, yp} = p, {xq, yq} = q) do
  #   lambda = div_int(sub(yq, yp), sub(xq, xp))
  #   get_r(lambda, p, q)
  # end

  # defp get_r(lambda, {xp, yp}, {xq, yq}) do
  #   xr = mult(lambda, lambda) |> sub(xp) |> sub(xq)
  #   yr = lambda |> mult(sub(xp, xr)) |> sub(yp)
  #   {xr, yr}
  # end

  # def point_double({xp, yp} = p) do
  #   a = @params.a |> Decimal.cast()
  #   lambda = mult(3, mult(xp, xp)) |> add(a) |> div_int(mult(2, yp))
  #   get_r(lambda, p, p)
  # end

  # def point_mul(p, 0), do: 0
  # def point_mul(p, 1), do: p
  # def point_mul(p, d) do
  #   if Decimal.rem(d, 2) == Decimal.cast(0) do
  #     point_add(p, point_mul(p, d |> sub(1)))
  #   else
  #     point_mul(point_double(p), d |> div_int(2))
  #   end
  # end

end
