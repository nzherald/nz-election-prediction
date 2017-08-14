#!/bin/sh
set -e

make

cp $(ls  | head -n1)/* /output
