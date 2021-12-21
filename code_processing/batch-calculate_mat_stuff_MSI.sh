#!/bin/bash

module load R/4.1.0

cd /home/feczk001/day00096/laterality-wta/ || exit

if [ ${#} -eq 1 ] ; then
    arm=${1}
else
    echo "Supply ARMS index: 1/2"
    exit 1
fi

echo "Calculating # runs"
n=0
for m in "data/mats-ARMS${arm}"/*.mat ; do
    if [ ! -e "${m//.mat/.rds}" ] || [ ! -e "${m//.mat/_sk.csv}" ] ; then
        n=$(( n + 1 ))
    fi
done

if [ ${n} -eq 0 ] ; then
    echo "No runs needed, exiting"
    exit
fi

echo "Found ${n} sessions to run"
echo "Starting runs ..."

for m in "data/mats-ARMS${arm}"/*.mat ; do

    if [ ! -e "${m//.mat/.rds}" ] || [ ! -e "${m//.mat/_sk.csv}" ] ; then

        # n=$(( n + 1 ))

        sbatch \
            --time=00:05:00         \
            -N1                     \
            --ntasks-per-node   1   \
            --mem-per-cpu       1gb \
            code_processing/calculate_mat_stuff_MSI.sh "${m}"

        # break

    fi

done