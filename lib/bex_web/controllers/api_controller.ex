defmodule BexWeb.ApiController do
  use BexWeb, :controller

  alias Bex.Wallet
  require Logger
  @storage "./files"

  plug :find_private_key

  @doc """
  Create a metanet directory or file.
  The private key is association with the APP_KEY in the header.

  params:
    dir: "a/b/c"
    file: %Plug.Upload{}

  if only dir, create a dir; if path and file, creat the file.
  Can not create dir or file under unexisted dir.
  """
  def create(conn, %{"dir" => dir, "file" => file}) do
    %{content_type: type, filename: filename, path: path} = file
    new_filename = UUID.uuid4()
    new_path = @storage <> "/" <> new_filename
    File.cp!(path, new_path)

    case Wallet.create_document(
           %{
             path: new_path,
             type: type,
             filename: filename,
             dir: dir,
             base_key_id: conn.assigns.private_key.id
           },
           conn.assigns.private_key
         ) do
      {:ok, _doc} ->
        # start_other_process_to_build_and_send_this_document(doc)
        text(conn, "ok")

      {:error, _} ->
        json(conn, %{error: "can not save this file"})
    end
  end

  def create(conn, %{"dir" => dir}) do
    IO.inspect(conn.assigns.private_key)

    case Wallet.create_document(
           %{
             type: "directory",
             dir: dir,
             base_key_id: conn.assigns.private_key.id
           },
           conn.assigns.private_key
         ) do
      {:ok, doc} ->
        Wallet.upload_document(doc)
        text(conn, "ok")

      {:error, _} ->
        json(conn, %{error: "can not create this dir"})
    end
  end

  def create(conn, _) do
    json(conn, %{error: "need dir"})
  end

  # {:error, msg} or {:ok, private_key}
  defp find_private_key(conn, _options) do
    case get_req_header(conn, "app_key") do
      [] ->
        conn
        |> json(%{error: "no app_key in headers"})
        |> halt()

      [app_key] ->
        case Wallet.find_private_key_by_app_key(app_key) do
          nil ->
            conn
            |> json(%{error: "app_key not exists"})
            |> halt()

          pk ->
            conn
            |> assign(:private_key, pk)
        end

      other ->
        conn
        |> json(%{error: "invalid APP_KEY: #{inspect(other)}"})
        |> halt()
    end
  end
end
