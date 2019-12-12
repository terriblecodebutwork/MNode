export USER=postgres 
export PASS=JAmHHVMLFKKvND8OJhkFYu 
export HOST=localhost 
export DATABASE=bex_prod 
export PORT=8880 
export DATABASE_URL=ecto://$USER:$PASS@$HOST/$DATABASE 
export SECRET_KEY_BASE=1irqCrVrCD5yoTlarJAmHHVMLFKKvND8OJhkFYuT3kOf1Ke1LVAmHdUyI7/+HfoS 
export BexAdmin=JAmHHVMLFKKvND8OJhkFYuT3 
export BexChat=false

MIX_ENV=prod iex -S mix 
