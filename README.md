# Hocket #

Hocket - The Haskell [pocket](http://getpocket.com/) client

## About ##

Hocket is a minimalistic [pocket](http://getpocket.com/) client that
works on the commandline (using
[vty-ui](http://jtdaugherty.github.com/vty-ui/)) written in Haskell.

It's minimalistic in the sense that it matches my personal workflow
with pocket, currently supported features:

- view unread pocket items
- archive items (mark as read)
- rename items

## Installation ##

Hocket can be installed using `cabal`.

Arguments to the executable:

- `gui` - starts the gui
- add <url> - adds the <url> to your pocket reading list and quits

## Authentication ##

Hocket requires a `hocket.cfg` file to authenticate you. Example:

```
[Credentials]
consumer_key = "12345-678912345678912345678912"
access_token = "abcdefgh-1234-abcd-1234-abcdef"

[Launch]
launch_cmd = firefox '%s'
```

Where `launch_cmd` is used to open a browser for the current pocket item.

## Is it done? ##

No ;). As said above it is fairly minimalistic to fit my workflow,
nevertheless contributions are welcome!

## Screenshot ##

(text blur was added post screenshot)

![Screenshot](/pics/hocket.png?raw=true "Hocket screenshot")
