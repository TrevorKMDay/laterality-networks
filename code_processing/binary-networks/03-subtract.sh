#!/bin/bash

module load workbench

mkdir -p subtract

flips=$(find flipped/ -name "flipped_*.nii")

n=0
for f in ${flips} ; do

    sub=$(echo "${f}" | grep -o "sub-NDARIN[A-Z0-9]*")
    s=$(find source/ -name "${sub}_*.nii")

    wb_command -cifti-math \
        "s-f"                                           \
        "subtract/${sub}_subtractionmap.dtseries.nii"   \
        -var s "${s}"                                   \
        -var f "${f}"

    n=$(( n + 1 ))

    echo "${n} ${sub}"

done