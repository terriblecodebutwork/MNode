defmodule Bex.WalletTest do
  use Bex.DataCase

  alias Bex.Wallet

  describe "private_keys" do
    alias Bex.Wallet.PrivateKey

    @valid_attrs %{address: "some address", bn: "some bn", from: 42, hex: "some hex"}
    @update_attrs %{
      address: "some updated address",
      bn: "some updated bn",
      from: 43,
      hex: "some updated hex"
    }
    @invalid_attrs %{address: nil, bn: nil, from: nil, hex: nil}

    def private_key_fixture(attrs \\ %{}) do
      {:ok, private_key} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Wallet.create_private_key()

      private_key
    end

    test "list_private_keys/0 returns all private_keys" do
      private_key = private_key_fixture()
      assert Wallet.list_private_keys() == [private_key]
    end

    test "get_private_key!/1 returns the private_key with given id" do
      private_key = private_key_fixture()
      assert Wallet.get_private_key!(private_key.id) == private_key
    end

    test "create_private_key/1 with valid data creates a private_key" do
      assert {:ok, %PrivateKey{} = private_key} = Wallet.create_private_key(@valid_attrs)
      assert private_key.address == "some address"
      assert private_key.bn == "some bn"
      assert private_key.from == 42
      assert private_key.hex == "some hex"
    end

    test "create_private_key/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Wallet.create_private_key(@invalid_attrs)
    end

    test "update_private_key/2 with valid data updates the private_key" do
      private_key = private_key_fixture()

      assert {:ok, %PrivateKey{} = private_key} =
               Wallet.update_private_key(private_key, @update_attrs)

      assert private_key.address == "some updated address"
      assert private_key.bn == "some updated bn"
      assert private_key.from == 43
      assert private_key.hex == "some updated hex"
    end

    test "update_private_key/2 with invalid data returns error changeset" do
      private_key = private_key_fixture()
      assert {:error, %Ecto.Changeset{}} = Wallet.update_private_key(private_key, @invalid_attrs)
      assert private_key == Wallet.get_private_key!(private_key.id)
    end

    test "delete_private_key/1 deletes the private_key" do
      private_key = private_key_fixture()
      assert {:ok, %PrivateKey{}} = Wallet.delete_private_key(private_key)
      assert_raise Ecto.NoResultsError, fn -> Wallet.get_private_key!(private_key.id) end
    end

    test "change_private_key/1 returns a private_key changeset" do
      private_key = private_key_fixture()
      assert %Ecto.Changeset{} = Wallet.change_private_key(private_key)
    end
  end

  describe "utxos" do
    alias Bex.Wallet.Utxo

    @valid_attrs %{index: 42, lock_script: "some lock_script", txid: "some txid", value: "120.5"}
    @update_attrs %{
      index: 43,
      lock_script: "some updated lock_script",
      txid: "some updated txid",
      value: "456.7"
    }
    @invalid_attrs %{index: nil, lock_script: nil, txid: nil, value: nil}

    def utxo_fixture(attrs \\ %{}) do
      {:ok, utxo} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Wallet.create_utxo()

      utxo
    end

    test "list_utxos/0 returns all utxos" do
      utxo = utxo_fixture()
      assert Wallet.list_utxos() == [utxo]
    end

    test "get_utxo!/1 returns the utxo with given id" do
      utxo = utxo_fixture()
      assert Wallet.get_utxo!(utxo.id) == utxo
    end

    test "create_utxo/1 with valid data creates a utxo" do
      assert {:ok, %Utxo{} = utxo} = Wallet.create_utxo(@valid_attrs)
      assert utxo.index == 42
      assert utxo.lock_script == "some lock_script"
      assert utxo.txid == "some txid"
      assert utxo.value == Decimal.new("120.5")
    end

    test "create_utxo/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Wallet.create_utxo(@invalid_attrs)
    end

    test "update_utxo/2 with valid data updates the utxo" do
      utxo = utxo_fixture()
      assert {:ok, %Utxo{} = utxo} = Wallet.update_utxo(utxo, @update_attrs)
      assert utxo.index == 43
      assert utxo.lock_script == "some updated lock_script"
      assert utxo.txid == "some updated txid"
      assert utxo.value == Decimal.new("456.7")
    end

    test "update_utxo/2 with invalid data returns error changeset" do
      utxo = utxo_fixture()
      assert {:error, %Ecto.Changeset{}} = Wallet.update_utxo(utxo, @invalid_attrs)
      assert utxo == Wallet.get_utxo!(utxo.id)
    end

    test "delete_utxo/1 deletes the utxo" do
      utxo = utxo_fixture()
      assert {:ok, %Utxo{}} = Wallet.delete_utxo(utxo)
      assert_raise Ecto.NoResultsError, fn -> Wallet.get_utxo!(utxo.id) end
    end

    test "change_utxo/1 returns a utxo changeset" do
      utxo = utxo_fixture()
      assert %Ecto.Changeset{} = Wallet.change_utxo(utxo)
    end
  end
end
