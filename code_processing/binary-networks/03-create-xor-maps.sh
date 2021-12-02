#!/bin/bash


module load workbench

mkdir -p xor

flips=$(find flipped/ -name "flipped_*.nii")

n=0
for f in ${flips} ; do

    sub=$(echo "${f}" | grep -o "sub-NDARIN[A-Z0-9]*")
    s=$(find source/ -name "${sub}_*.nii")

    wb_command -cifti-math \
        "(s > 0) && (f == 0)"           \
        "xor/${sub}_xor.dtseries.nii"   \
        -var s "${s}"                   \
        -var f "${f}"

    n=$(( n + 1 ))

    if [ ${n} -eq 100 ] ; then break ; fi

done