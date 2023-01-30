#!/bin/bash

Rscript saatli-maarif.R
python3 update_screen.py
rm -f *.bmp *.pdf

