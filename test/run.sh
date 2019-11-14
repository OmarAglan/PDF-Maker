#!/bin/sh

if [ -z "$(command -v realpath)" ]; then
    echo "realpath not found."
    exit
fi

URL='http://en.wikipedia.org/wiki/Asymptote_(vector_graphics_language)'
SOURCEDIR="$(realpath "$0")"
SOURCEDIR="${SOURCEDIR%/*}"

case $1 in
    latex)
        URL='http://en.wikibooks.org/wiki/User:Dirk_H%C3%BCnniger/latex'
        shift 1;;
    lua)
        URL='http://en.wikipedia.org/wiki/Lua_(programming_language)'
        shift 1;;
esac

[ ! -d ~/latex-tree ] && mkdir ~/latex-tree

"$SOURCEDIR"/../dist/build/mediawiki2latex/mediawiki2latex -o output.pdf \
    -u "$URL" -c ~/latex-tree "$@"
