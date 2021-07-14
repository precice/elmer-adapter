#!/bin/sh
set -e -u

while getopts ":dn" opt; do
  case ${opt} in
  d)
    python3 heat.py -d --error-tol 10e-2
    ;;
  n)
    python3 heat.py -n --error-tol 10e-2
    ;;
  \?)
    echo "Usage: cmd [-d] [-n]"
    ;;
  esac
done
