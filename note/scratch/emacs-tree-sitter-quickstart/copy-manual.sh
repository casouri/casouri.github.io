#!/bin/bash

# To build manual:
# make elisp.html HTML_OPTS="--html --css-ref=./manual.css"

MANUAL_DIR="/Users/yuan/emacs/doc/lispref/elisp.html"

cp "${MANUAL_DIR}/Parsing-Program-Source.html" ./manual
cp "${MANUAL_DIR}/Language-Definitions.html" ./manual
cp "${MANUAL_DIR}/Using-Parser.html" ./manual
cp "${MANUAL_DIR}/Retrieving-Node.html" ./manual
cp "${MANUAL_DIR}/Accessing-Node.html" ./manual
cp "${MANUAL_DIR}/Pattern-Matching.html" ./manual
cp "${MANUAL_DIR}/Multiple-Languages.html" ./manual
cp "${MANUAL_DIR}/Tree_002dsitter-C-API.html" ./manual

cp -f "${MANUAL_DIR}/Parser_002dbased-Font-Lock.html" ./manual
cp -f "${MANUAL_DIR}/Parser_002dbased-Indentation.html" ./manual
cp -f "${MANUAL_DIR}/List-Motion.html" ./manual
