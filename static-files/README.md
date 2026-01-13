# Static Files Directory

This directory is served by a Python HTTP server on port 8081.

## Usage

Place any static files (HTML, CSS, JavaScript, images, etc.) in this directory or its subdirectories, and they will be accessible at:

```
http://localhost:8081/<filename>
```

## Examples

- `static-files/hello.html` → http://localhost:8081/hello.html
- `static-files/css/style.css` → http://localhost:8081/css/style.css
- `static-files/images/logo.png` → http://localhost:8081/images/logo.png

## Server Info

The server automatically starts when you enter the nix development shell and serves everything in this directory and all subdirectories.

