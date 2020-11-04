#!/bin/bash
set -o errexit
set -o xtrace

pushd "${WORKDIR}"
Rscript runner.R
popd
