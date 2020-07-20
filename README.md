# elm-kodirpc

This is a minimal example of how to connect to a websocket and decode responses. The decoder is written to handle Kodi JSON-RPC responses. It uses port 9090 by default.

![demo](https://github.com/ionif/elm-kodirpc/blob/master/demo.gif)

## Building and running locally

```
elm make src/Main.elm --output=elm.js
elm reactor
```

## Development task list
- [X] Basic string decoder
  - {"result" : String}
- [ ] JSON Introspect decoder
  - handles Introspect calls
- [ ] params decoder
  - {"params" : Object}
- [ ] List decoder
  - {"result" : [{},{}]}
