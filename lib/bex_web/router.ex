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
    resources "/private_keys", PrivateKeyController
    resources "/utxos", UtxoController
    live "/index", IndexLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", BexWeb do
  #   pipe_through :api
  # end
end
