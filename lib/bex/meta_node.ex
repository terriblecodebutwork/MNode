defmodule Bex.MetaNode do
  # manage the metanet nodes content
  alias Bex.Wallet
  import Ecto.Query
  alias Bex.Repo
  alias Bex.Wallet.Utxo
  alias BexLib.Script

  def get_node(base_key, dir) do
    case Wallet.find_txids_with_dir(base_key, dir) do
      {:ok, []} ->
        nil

      # FIXME handle multi-version
      {:ok, [txid | _]} ->
        get_utxo_data(txid)
    end
  end

  defp get_utxo_data(txid) do
    query =
      from u in Utxo,
        where: u.txid == ^txid and u.type == "data"

    case Repo.one(query) do
      %Utxo{lock_script: l} ->
        Script.parse(l)
        |> drop_metanet_metadata()

      _ ->
        nil
    end
  end

  defp drop_metanet_metadata([_, :OP_RETURN, "meta", _, _ | contents]) do
    contents
  end

  defp drop_metanet_metadata([:OP_RETURN, "meta", _, _ | contents]) do
    contents
  end
end
