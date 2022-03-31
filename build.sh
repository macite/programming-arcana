#!/usr/bin/env bash

cd tools/syntax-parser
python3 eebnf_exporter.py ../../syntax ../../syntax-out no

cd ../..
latexmk -pdf
