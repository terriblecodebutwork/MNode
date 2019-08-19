defmodule Bex.Repo do
  use Ecto.Repo,
    otp_app: :bex,
    adapter: Ecto.Adapters.Postgres
end
