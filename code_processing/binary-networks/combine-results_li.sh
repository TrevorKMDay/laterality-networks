#!/bin/bash

cd /home/feczk001/day00096/laterality/WTA-networks/results_li || exit

for i in sub-*.csv ; do

    sub=${i%.csv}

    for line in $(tail -n16 "${i}") ; do
        echo "${sub},${line}"
    done

done