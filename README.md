# Hocket #

Hocket - The Haskell [Raindrop.io](https://raindrop.io/) client

[![Build Status](https://travis-ci.org/markus1189/hocket.png?branch=master)](https://travis-ci.org/markus1189/hocket)

## About ##

Hocket is a minimalistic [Raindrop.io](https://raindrop.io/) client that
works on the commandline (using
[brick](https://github.com/jtdaugherty/brick)) written in Haskell.

It's minimalistic in the sense that it matches my personal workflow
with Raindrop.io, currently supported features:

- view unread bookmark items
- archive items (move to special collection)

## Installation ##

Hocket can be installed using `nix`.

## Authentication ##

Hocket requires a `config.dhall` file to authenticate you. Example:

```
{
  _raindropToken = "your-raindrop-test-token-here"
}
```

## How to get a Raindrop.io Test Token ##

1. Go to your Raindrop.io settings: https://app.raindrop.io/settings/integrations
2. Click on "+ Create new app"
3. Give it a name (e.g., "Hocket")
4. Click "Create"
5. You'll see your app listed. Click on it
6. Copy the "Test token" - this is what you'll use for `_raindropToken` in your `config.dhall`

## Is it done? ##

No ;). As said above it is fairly minimalistic to fit my workflow,
nevertheless contributions are welcome!
