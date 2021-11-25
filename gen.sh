#!/bin/bash

set -e

TAG="$1"
OUT="generated-client"

if [ "$TAG" = "" ]; then
    echo "Tag name is missing"
    exit 1
fi

curl --fail https://raw.githubusercontent.com/ory/hydra/master/docs/versioned_docs/version-$TAG/.static/api.json > api.swagger.json

openapi-generator-cli generate -i api.swagger.json -g haskell-http-client -o $OUT -c config.yml

cp LICENSE $OUT/
