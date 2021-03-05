#!/bin/bash

set -e

if [ "$1" = "" ]; then
    echo "Tag name is missing"
    exit 1
fi
TAG="$1"

curl --fail https://raw.githubusercontent.com/ory/hydra/$TAG/.schema/api.swagger.json > api.swagger.json

openapi-generator-cli generate -i api.swagger.json -g haskell-http-client -o ory-hydra-client -c config.yml

cp LICENSE ory-hydra-client/
