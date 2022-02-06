#!/bin/bash

# sudo port install py39-fonttools
pyftsubset "$1" --text-file=日本語.txt --unicodes=3000-303f --layout-features="*" --flavor=woff2
