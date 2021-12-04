#!/bin/bash

cd /home/feczk001/day00096/laterality-wta/code_processing/binary-networks || \
    exit

echo "sub,$(head -n1 "$(find results_li/ -name "sub-*.csv" | head -n1)")" > \
    results_li.csv

for i in results_li/sub-*.csv ; do

    sub=${i%.csv}

    for line in $(tail -n16 "${i}") ; do
        echo "${sub},${line}" >> results_li.csv
    done

done