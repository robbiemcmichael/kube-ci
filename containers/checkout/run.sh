#!/bin/sh

set -e
set -u
set -x

git init
git config remote.origin.url "$REPO"
git fetch
git checkout -b "$BRANCH" "$COMMIT"
