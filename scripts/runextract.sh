#!/bin/bash
set -e

source scripts/runsetup.sh
export PYTHONPATH=$PYTHONPATH:src

FILE=.env.test
if [ -f "$FILE" ]; then
    source $FILE
fi

echo "$@"

python -m wikiextract.extractor "$@"
