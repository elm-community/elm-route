# Example app

This is a simple example demonstrating client-side routing using the
`elm-route` parser. This builds upon the `app` example by adding query
string parsing into the mix. To run it locally do the following after
cloning the repository:

``` shell
$ elm make Main.elm --output=elm.js
$ python3 server.py
```

Visit http://localhost:8000 in your browser.
