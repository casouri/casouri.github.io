#!/bin/bash

# sudo port install py39-fonttools
pyftsubset "$1" --unicodes=D7B0-D7FF --layout-features="*" --flavor=woff2
