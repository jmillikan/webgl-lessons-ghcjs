#!/bin/bash

buildEx() {
    cabal configure --ghcjs
    cabal build $2
    cp $1 dist/build/$2/$2.jsexe/index.html
    cp src/glMatrix-0.9.5.min.js dist/build/$2/$2.jsexe/glMatrix-0.9.5.min.js
}

if [ -z $1 ] 
  then 
    buildEx src/index.html ghcjs-webgl-lesson01
    buildEx src/index.html ghcjs-webgl-lesson02
    buildEx src/index.html ghcjs-webgl-lesson03
else
  buildEx $1 $2
fi
