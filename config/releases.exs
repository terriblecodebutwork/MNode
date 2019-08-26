import Config

config :bex, BexWeb.Endpoint,
  server: true,
  url: [host: System.get_env("APP_NAME") <> ".gigalixirapp.com", port: 443]
