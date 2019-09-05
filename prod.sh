export USER=postgres
export PASS=postgres
export HOST=localhost
export DATABASE=bex_prod
export PORT=80
export DATABASE_URL=ecto://$USER:$PASS@$HOST/$DATABASE
export SECRET_KEY_BASE=1irqCrVrCD5yoTlarJAmHHVMLFKKvND8OJhkFYuT3kOf1Ke1LVAmHdUyI7/+HfoS

MIX_ENV=prod mix phx.server
