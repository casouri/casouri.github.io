#!/bin/bash

# sudo port install py39-fonttools
pyftsubset "$1" --text-file=简繁常用字及符号.txt --unicodes=3000-303f --layout-features="*" --flavor=woff2
