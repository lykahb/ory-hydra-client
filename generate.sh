#!/bin/bash

set -e

TAG="$1"
OUT="generated-client"

if [ "$TAG" = "" ]; then
    echo "Tag name is missing"
    exit 1
fi
TEMP_FILE=`mktemp /tmp/api.swagger.XXXX.json`

curl --fail https://raw.githubusercontent.com/ory/hydra/$TAG/spec/api.json > $TEMP_FILE

openapi-generator-cli generate -i $TEMP_FILE -g haskell-http-client -o $OUT -c config.yml

cp LICENSE $OUT/
