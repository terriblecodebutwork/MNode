defmodule BexWeb.Router do
  use BexWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug Phoenix.LiveView.Flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  @admin_scope System.get_env("BexAdmin", "/admin")

  scope "/", BexWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/txrepo", PageController, :show_tx
    live "/parser", ParserLive
    live "/parser/:rawtx", ParserLive
    get "/ad", PageController, :ad
    get "/gun", PageController, :gun
    get "/chat", PageController, :chat
    get "/merkle", PageController, :merkle_page
  end

  scope "/mnode", BexWeb do
    pipe_through :browser

    scope(@admin_scope) do
      live "/", IndexLive
      get "/txrepo", PageController, :show_tx
      live "/meta", MetaLive
      resources "/keys", PrivateKeyController
    end

    # resources "/utxos", UtxoController
    # get "/m2/:net/deposit/:tx", DepositController, :deposit
  end

  # Other scopes may use custom stacks.
  scope "/api", BexWeb do
    pipe_through :api

    post "/write", WriteController, :write
    post "/mkdir", WriteController, :mkdir
    get "/read", ReadController, :read
    post "/transfer", TransferController, :transfer

    # post "/webhook/154814876", HookController, :mb_hook

    get "/merkle/:txid", PageController, :merkle_path
  end
end
