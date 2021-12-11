library(tidyverse)
library(raveio)
library(ciftiTools)

args <- commandArgs(trailingOnly = TRUE)

args <- c("D:/workbench-windows64-v1.4.2/workbench/bin_windows64",
          "C:/Users/Trevor/Desktop/sub-NDARINV003RTV85_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative.dtseries.nii",
          "sub-NDARINV00BD7VDC.mat",
          "foo.dtseries.nii")

wb <- args[1]
template <- args[2]
input_mat <- args[3]
output <- args[4]

ciftiTools.setOption("wb_path", wb)

###############################################################################

eta2vox <- read_mat(input_mat)$eta_to_template_vox
template <- read_cifti(template)

output_cifti <- template
output_cifti$data$cortex_left <- eta2vox[1:29696, ]
output_cifti$data$cortex_right <- eta2vox[29697:59412, ]

write_cifti(output_cifti, output)
