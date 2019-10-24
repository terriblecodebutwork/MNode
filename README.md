# M2

## Make Directory
```
POST `/api/mkdir`

headers:
APP_KEY: <key>
onchain_path: /user123
```

```bash
curl -X POST  \
  'http://localhost:4000/api/mkdir' \
  -H 'app_KEY: TJAdn4wBhMJi+wHvVpLXquwaCD+2lQuVNad/PaIkbnA=' \
  -H 'onchain_path: /elixir'
```


## Write File
```
POST `/api/write`

headers:
APP_KEY: <key>.
Content-Type: application/octet-stream
onchain_path: /user123/lol.png
```

```bash
curl -X POST  \
  'http://localhost:4000/api/write' \
  -H 'app_KEY: TJAdn4wBhMJi+wHvVpLXquwaCD+2lQuVNad/PaIkbnA=' \
  -H 'onchain_path: /elixir/mix.exs' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary @mix.exs
```