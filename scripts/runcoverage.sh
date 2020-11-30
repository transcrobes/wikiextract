#!/bin/bash
set -e

source scripts/runsetup.sh
export PYTHONPATH=$PYTHONPATH:tests

FILE=.env.test
if [ -f "$FILE" ]; then
    source $FILE
fi

# FIXME: TO BE COMPLETED

coverage run --source='src' src/wikiextract/extractor.py

coverage report -m --skip-covered --skip-empty --fail-under 66
