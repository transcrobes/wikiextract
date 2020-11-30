#!/bin/bash
set -e

source scripts/runsetup.sh
export PYTHONPATH=$PYTHONPATH:tests:src

FILE=.env.test
if [ -f "$FILE" ]; then
    source $FILE
fi

pytest

return

pylint --ignore requirements.txt,requirements.ci.txt src/*
pylint tests/*

# pre-commit also has flake8 linter
pre-commit run --all-files --verbose

coverage run --source='src' src/manage.py test --verbosity=1 tests $1

coverage report -m --skip-covered --skip-empty --fail-under 60
