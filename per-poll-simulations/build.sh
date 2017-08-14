#!/bin/sh
set -e

make

cp -r $(ls  | head -n1)/*.{csv,txt} /output
