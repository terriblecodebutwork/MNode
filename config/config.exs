# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :bex,
  ecto_repos: [Bex.Repo]

# Configures the endpoint
config :bex, BexWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "diab0zU2l9dnlDCHzYZHX6JEw8k8G6cGZ0vuYOvTgPTHHK58Z+oBQK0/ETWP5kaP",
  render_errors: [view: BexWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Bex.PubSub, adapter: Phoenix.PubSub.PG2],
  live_view: [
    signing_salt: "Gdgg5um6ZrEWSK91V50DypmKA6D/mv+8"
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configures SvApi
config :sv_api, mods: [SvApi.Bitindex]

config :sv_api,
  bitindex_api_key: "44UFrLxSBgPxt4mibqw9m9voHps7RbgT1j92YE1K7XUKefBPLMiPXq7e5Lrmpp8NWa"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
