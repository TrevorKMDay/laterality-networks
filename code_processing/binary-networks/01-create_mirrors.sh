#!/bin/bash

module load workbench

cwd=/home/feczk001/day00096/laterality-wta/code_processing/binary-networks
code=/home/feczk001/day00096/laterality/code

cd ${cwd} || exit

# Data moved to Anita's share
source_dir=/home/rando149/shared/projects/ABCD_net_template_matching/ABCD_templ_matched_scalars_group1_10_min_overlap

mkdir -p source flipped

echo -n "Symlinking sources to here ... "
for i in "${source_dir}"/*_recolored.dtseries.nii ; do

    bn=$(basename "${i}")

    if [ ! -e "source/${bn}" ] ; then
        ln -s "${i}" "source/${bn}"
    fi

done
echo " done."

n=0
for i in source/*.nii ; do

    flip="flipped/flipped_$(basename "${i}")"

    if [ ! -e "${flip}" ] ; then
        ${code}/bash/create_mirror.sh "${i}" "${flip}"
    fi

    break

    n=$(( n + 1 ))
    # if [ ${n} -eq 10 ] ; then break ; fi

done

# Clean up
# Changed to create these files in /tmp
# rm flipped/*.[lr]h.func.gii