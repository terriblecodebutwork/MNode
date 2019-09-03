# MNode

An API based Metanet service.

## import root private key

Start the server, and open "http://localhost:4000/index".

Input the hex string format of your private key.

## UTXOs manager

we defined 3 types of utxos based on value:

1. gold: > 90k satoshi
2. coin: = 90k satoshi
3. dust: < 90k satoshi

- and "permission", 546 satoshi used for create new metanode.

## APIs

The parameters need be JSON.

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

If is root node, set parent to `false`.

We use `path` or `id` and the root private key to generate the private key of this node. Highly recommend to store the `path` or `id` in the content.

The content could be a string or an array of string, if that is an array of string, each element will be pushed saparately.

The same as Unix file system, you need create the parent path before create the children paths.

- Find the txids of a Metanet node
`GET ../api/mnode`

```json
{
  "path": "foo/bar/baz"
}
```

or

```json
{
  "id": 1
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
- can't specify the version of parent node now. All new node will be added under the lateset version of parent node.