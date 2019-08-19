use Mix.Config

# Configure your database
config :bex, Bex.Repo,
  username: "postgres",
  password: "postgres",
  database: "bex_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :bex, BexWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn
