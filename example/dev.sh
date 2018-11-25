#!/bin/sh

nodemon -e elm --exec "elm make src/Main.elm --output=dist/index.js" & live-server --wait=200