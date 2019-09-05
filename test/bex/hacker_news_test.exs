defmodule Bex.HackerNewsTest do
  use Bex.DataCase

  alias Bex.HackerNews

  describe "posts" do
    alias Bex.HackerNews.Post

    @valid_attrs %{content: "some content", name: "some name"}
    @update_attrs %{content: "some updated content", name: "some updated name"}
    @invalid_attrs %{content: nil, name: nil}

    def post_fixture(attrs \\ %{}) do
      {:ok, post} =
        attrs
        |> Enum.into(@valid_attrs)
        |> HackerNews.create_post()

      post
    end

    test "list_posts/0 returns all posts" do
      post = post_fixture()
      assert HackerNews.list_posts() == [post]
    end

    test "get_post!/1 returns the post with given id" do
      post = post_fixture()
      assert HackerNews.get_post!(post.id) == post
    end

    test "create_post/1 with valid data creates a post" do
      assert {:ok, %Post{} = post} = HackerNews.create_post(@valid_attrs)
      assert post.content == "some content"
      assert post.name == "some name"
    end

    test "create_post/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = HackerNews.create_post(@invalid_attrs)
    end

    test "update_post/2 with valid data updates the post" do
      post = post_fixture()
      assert {:ok, %Post{} = post} = HackerNews.update_post(post, @update_attrs)
      assert post.content == "some updated content"
      assert post.name == "some updated name"
    end

    test "update_post/2 with invalid data returns error changeset" do
      post = post_fixture()
      assert {:error, %Ecto.Changeset{}} = HackerNews.update_post(post, @invalid_attrs)
      assert post == HackerNews.get_post!(post.id)
    end

    test "delete_post/1 deletes the post" do
      post = post_fixture()
      assert {:ok, %Post{}} = HackerNews.delete_post(post)
      assert_raise Ecto.NoResultsError, fn -> HackerNews.get_post!(post.id) end
    end

    test "change_post/1 returns a post changeset" do
      post = post_fixture()
      assert %Ecto.Changeset{} = HackerNews.change_post(post)
    end
  end
end
