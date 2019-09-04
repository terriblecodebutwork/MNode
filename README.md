# MNode

An API based Metanet service.

## import root private key

Start the server, and open "http://localhost:4000/nodes/new" to import the root private key and get APP_KEY.

## UTXOs manager

we defined 3 types of utxos based on value (You can set the X value):

1. gold: > X satoshi
2. coin: = X satoshi
3. dust: < X satoshi

- and "permission", 546 satoshi used for create new metanode.

MetaNet node creation will only use the coins as inputs. If run out of coins, MNode will auto recast the dusts into coins.

You also can manually manage utxos in "http://localhost:4000/dashboradx" .

**DEPOSIT**: After deposit, you need "resync utxos" by yourself.

## APIs

Set `APP_KEY` in the request header.

The parameters need be JSON.

- Create a Metanet node
`POST ../api/mnode`

Ensure every node will have a unique ID

```json
{
  "parent": "0",
  "name": "1",
  "content": []
}
```

All `id` should be string.

If is root node, set parent to `false`.

We use `name` and the root private key to generate the private key of this node. Highly recommend to store the `name` in the content.

The content could be a string or an array of string, if that is an array of string, each element will be pushed saparately.

Also support `path` version:
```json
{
  "path": "foo/bar",
  "content": []
}
```

The same as Unix file system, you need create the parent path before create the children paths.

- Find the txids of a Metanet node
`GET ../api/mnode`

```json
{
  "name": "1"
}
```

or

```json
{
  "path": "foo"
}

## Install the release

- macOS:
- Ubuntu:

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