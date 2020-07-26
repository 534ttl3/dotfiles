#!/bin/bash

# $1: base directory for find, $2: input into fzf

fzf_directory_one_shot() {
    (cd ~ && local dir && dir=$(find "$1" -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf +m --no-sort -f "$2") && echo "$dir")
}

fzf_directory_one_shot "$1" "$2"

# $(realpath --relative-to="$HOME" "${PWD}/public")
