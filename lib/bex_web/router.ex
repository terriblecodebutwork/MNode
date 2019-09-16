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

  scope "/", BexWeb do
    pipe_through :browser

    get "/", PageController, :index
    resources "/nodes", PrivateKeyController
    # resources "/utxos", UtxoController
    live "/dashboardx", IndexLive
    # live "/meta", MetaLive
    live "/meta/:id", MetaLive
  end

  # Other scopes may use custom stacks.
  scope "/api", BexWeb do
    pipe_through :api

    post "/mnode", ApiController, :create
    get "/mnode", ApiController, :find
    post "/webhook/154814876", HookController, :mb_hook
  end
end
