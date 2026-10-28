#!/bin/sh
set -e
cd "$(dirname "$0")/.."
steel tests/util-test.scm
