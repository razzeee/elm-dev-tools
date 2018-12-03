#!/bin/sh

nodemon --watch ../src --watch src --ext elm --exec "elm make src/Main.elm --output=dist/index.js" & live-server --watch=./dist