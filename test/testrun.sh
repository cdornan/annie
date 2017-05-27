#!/bin/bash
cd "`dirname "$0"`"

stack exec annie -- --help

stack exec annie -- --scan hello.txt

stack exec annie -- --list

stack exec annie -- --map hello.txt

cat hello.txt.anon
