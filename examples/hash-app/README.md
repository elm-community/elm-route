# Example app

This is a simple example demonstrating client-side routing using the
`elm-route` parser. This is similar to the `app` example except that
it uses `elm-history`'s `hash` signal rather than its `path` signal
for routing. To run it locally do the following after cloning the
repository:

``` shell
$ elm make Main.html --output=elm.js
$ python3 server.py
```

Visit http://localhost:8000 in your browser.
