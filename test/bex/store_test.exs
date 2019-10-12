defmodule Bex.StoreTest do
  use Bex.DataCase

  alias Bex.Store

  describe "merkle" do
    alias Bex.Store.Merkle

    @valid_attrs %{left: "some left", right: "some right", top: "some top"}
    @update_attrs %{
      left: "some updated left",
      right: "some updated right",
      top: "some updated top"
    }
    @invalid_attrs %{left: nil, right: nil, top: nil}

    def merkle_fixture(attrs \\ %{}) do
      {:ok, merkle} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Store.create_merkle()

      merkle
    end

    test "list_merkle/0 returns all merkle" do
      merkle = merkle_fixture()
      assert Store.list_merkle() == [merkle]
    end

    test "get_merkle!/1 returns the merkle with given id" do
      merkle = merkle_fixture()
      assert Store.get_merkle!(merkle.id) == merkle
    end

    test "create_merkle/1 with valid data creates a merkle" do
      assert {:ok, %Merkle{} = merkle} = Store.create_merkle(@valid_attrs)
      assert merkle.left == "some left"
      assert merkle.right == "some right"
      assert merkle.top == "some top"
    end

    test "create_merkle/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Store.create_merkle(@invalid_attrs)
    end

    test "update_merkle/2 with valid data updates the merkle" do
      merkle = merkle_fixture()
      assert {:ok, %Merkle{} = merkle} = Store.update_merkle(merkle, @update_attrs)
      assert merkle.left == "some updated left"
      assert merkle.right == "some updated right"
      assert merkle.top == "some updated top"
    end

    test "update_merkle/2 with invalid data returns error changeset" do
      merkle = merkle_fixture()
      assert {:error, %Ecto.Changeset{}} = Store.update_merkle(merkle, @invalid_attrs)
      assert merkle == Store.get_merkle!(merkle.id)
    end

    test "delete_merkle/1 deletes the merkle" do
      merkle = merkle_fixture()
      assert {:ok, %Merkle{}} = Store.delete_merkle(merkle)
      assert_raise Ecto.NoResultsError, fn -> Store.get_merkle!(merkle.id) end
    end

    test "change_merkle/1 returns a merkle changeset" do
      merkle = merkle_fixture()
      assert %Ecto.Changeset{} = Store.change_merkle(merkle)
    end
  end
end
