#!/bin/sh
PATTERN="$1"
FILE="$2" #TODO: /path/to/file
find . -iname $FILE |while read pdf_file;
do
    echo "===FILE:$pdf_file==="
    pdftotext "$pdf_file" - |grep $PATTERN -nr
done
