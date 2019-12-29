# M2

(WARNING) this repo is under developping and lack of testing.

## How to use?

### requirements

erlang  > 21
elixir > 1.5
postgreSQL > 9.5
nodejs

### run

```
iex -S mix phx.server
```

## Make Directory

```bash
curl -X POST  \
  'http://localhost:4000/api/mkdir' \
  -H 'x-app-key: 123' \
  -H 'x-onchain-path: /eva'
```


## Write File

```bash
curl -X POST  \
  'http://localhost:4000/api/write' \
  -H 'x-app-key: 123' \
  -H 'x-onchain-path: /eva/narv.png' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary @narv.png
```

## Read File

```bash
curl -X GET \
  'http://localhost:4000/api/read' \
  -H 'x-app-key: 123' \
  -H 'x-onchain-path: /joe_armstrong_crypto_tutorial.pdf' \
  -o joe_armstrong_crypto_tutorial.pdf
```

## Transfer Coins

```bash
curl -X POST 'http://localhost:4000/api/transfer' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'x-app-key: 3yJJyCN3Ykkg8fLRz4tmUUZ0FrlnHiXJWQlkycdzxIg=' \
  -d '{"to": "15Q1Max442hdV8TXzJHwgSs5aavYqS5kU2", "amount": 5000}'
```