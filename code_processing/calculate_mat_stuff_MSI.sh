#!/bin/bash

module load R/4.1.0 workbench

if [ ${#} -le 2 ] ; then
    the_mat="${1}"
    copy="${2}"
else
    echo "Supply input .mat"
    exit 1
fi

if [[ "${copy}" == 1 ]] ; then
    echo "Copy set"
else
    echo "Copy not set"
fi

code_proc=/home/feczk001/day00096/laterality-wta/code_processing

sub=$(echo "${the_mat}" | grep -o "sub-[A-Z0-9]*")
echo "Subj. ID: ${sub}"

template_dir="${code_proc}/binary-networks/source/"
template="sub-NDARINVZZNX6W2P_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative_recolored.dtseries.nii"

mat_cifti=$(mktemp /tmp/XXXX.dtseries.nii)
mat_cifti_flipped=${mat_cifti//.dtseries/_flipped.dscalar}

##############################################################################

echo
echo "Converting matrix to cifti to be flipped ..."
Rscript \
    ${code_proc}/mat_to_cifti.R   \
        "$(which wb_command)"                                               \
        "${template_dir}/${template}"                                       \
        "${the_mat}"                                                        \
        "${mat_cifti}"

echo "Done with converting mat to CIFTI: ${mat_cifti}"
echo

##############################################################################

echo
echo "Calculating skew/kurtosis"

Rscript \
    ${code_proc}/skew_and_kurtosis.R    \
        "$(which wb_command)"           \
        "${mat_cifti}"                  \
        "${the_mat//.mat/_sk.csv}"      \
        "${sub}"

echo "Done with ${the_mat//.mat/_sk.csv}"

##############################################################################

echo "Flipping ..."
/home/feczk001/day00096/laterality/code/bash/create_mirror.sh   \
    "${mat_cifti}"                                              \
    "${mat_cifti_flipped}"

echo "${mat_cifti_flipped}"
echo

# Copy files locally if flag is set
if [[ "${copy}" == 1 ]] ; then
    cp "${mat_cifti}" "${mat_cifti_flipped}" .
fi

##############################################################################

rds=${the_mat//.mat/.rds}

# Skip this last bit if rds already exists
if [ -e "${rds}" ] ; then exit ; fi

mask_LR=$(find ${code_proc}/binary-networks/source/ \
            -name "${sub}_*dtseries.nii")
mask_RL=$(find ${code_proc}/binary-networks/flipped/ \
            -name "flipped_${sub}_*dtseries.nii")

echo "LR mask: ${mask_LR}"
echo "RL mask: ${mask_RL}"

Rscript \
    ${code_proc}/calculate_mat_stuff_MSI.R  \
        "$(which wb_command)"               \
        "${mat_cifti}"                      \
        "${mat_cifti_flipped}"              \
        "${mask_LR}"                        \
        "${mask_RL}"                        \
        "${rds}"

echo "Done with saving out data ${rds}"
echo