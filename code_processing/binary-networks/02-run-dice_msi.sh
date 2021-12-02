#!/bin/bash

module load workbench R/4.0.4
wb=$(which wb_command)

subs=$(find flipped/ -name "*.nii" -exec basename {} \; |
        grep -o "sub-[[:alnum:]]*")

echo "Found $(echo "${subs}" | wc -w) subs"

for s in ${subs} ; do

    orig_file=$(find source/ -name "*${s}*.nii")
    flip_file=$(find flipped/ -name "*${s}*.nii")
    result=results_li/${s}.csv

    if [ $(echo "${orig_file} ${flip_file}" | wc -w) -eq 1 ] ; then
        echo "${s} only one file"
        continue
    fi

    if [ -e "${result}" ] ; then
        # echo "${s} is done"
        continue
    fi

    # echo Rscript dice_msi_individual.R "${wb}" "${orig_file}" "${flip_file}" "${result}"
    # break

    sbatch <<-EOF
#!/bin/bash
#SBATCH --time=00:05:00
#SBATCH --mem=4G
Rscript dice_msi_individual.R "${wb}" "${orig_file}" "${flip_file}" "${result}"
EOF

#     break

done