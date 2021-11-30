#!/bin/bash

module load R/4.1.0

cd /home/feczk001/day00096/laterality/ || exit

for m in mats-2633/*.mat ; do

    if [ ! -e "${m//mat/rds}" ] ; then

        sbatch \
            --time=00:05:00         \
            -N1                     \
            --ntasks-per-node   1   \
            --mem-per-cpu       1gb \
            code/calculate_mat_stuff_MSI.sh "${m}"

    fi

done