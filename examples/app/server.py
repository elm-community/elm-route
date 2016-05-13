#!/usr/bin/env python3
from http.server import HTTPServer, BaseHTTPRequestHandler
from time import sleep


class Handler(BaseHTTPRequestHandler):
    def serve_file(self, filename, content_type="text/html"):
        self.send_response(200)
        self.send_header("Content-type", content_type)
        self.end_headers()

        with open(filename, "rb") as f:
            self.wfile.write(f.read())

    def do_GET(self):
        if self.path == "/api/posts":
            sleep(1)
            return self.serve_file("posts.json", "application/json")
        if self.path == "/elm.js":
            return self.serve_file("elm.js", "application/javascript")
        return self.serve_file("index.html")

print("Listening on port 8000...")
httpd = HTTPServer(("", 8000), Handler)
httpd.serve_forever()
