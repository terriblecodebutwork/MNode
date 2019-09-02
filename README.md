# MNode

An API based Metanet service.

## import root private key

Start the server, and open "http://localhost:5101".

Input the hex string format of your private key.

## APIs

The parameters can be form-data or JSON.

- Create a Metanet node
`POST ../api/mnode`

```json
{
  "path": "foo/bar/baz",
  "broadcast": false,
  "content": [
    "1",
    "2",
    "3"
  ]
}
```

If ensure every node will have a unique ID, can use another schema:

```json
{
  "parent": 1,
  "id": 2,
  "broadcast": false,
  "content": []
}
```

All `id` will be converted to string.

We use `path` or `id` and the root private key to generate the private key of this node.

The content could be a string or an array of string, if that is an array of string, each element will be pushed saparately.

The same as Unix file system, you need create the parent path before create the children paths.

- Find the txids of a Metanet node
`GET ../api/mnode`

```json
{
  "path": "foo/bar/baz"
}
```

## Install the release

- macOS:
- Ubuntu16:

## Build from source

- elixir >= 1.9
- phoenix >= 1.4
- postgresql >= 9.6

```sh
mix deps.get
mix ecto.setup
cd assets && npm install
cd .. && mix phx.server
```

## known bugs

- if broadcast failed, the utxos in database will be different with blockchain. So please ensure broadcast successed if you want to braodcast.