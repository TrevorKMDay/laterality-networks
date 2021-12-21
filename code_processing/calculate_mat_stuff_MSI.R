# setwd("G:/My Drive/Research/laterality/laterality-wta/code_processing//")

suppressPackageStartupMessages(library(tidyverse))
library(raveio)
library(ciftiTools)

# args <- c("D:/workbench-windows64-v1.4.2/workbench/bin_windows64",
#           "C:/Users/Trevor/Desktop/hO2t.dtseries.nii",
#           "C:/Users/Trevor/Desktop/hO2t_flipped.dscalar.nii",
#           "C:/Users/Trevor/Desktop/sub-NDARINV003RTV85_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative.dtseries.nii",
#           "C:/Users/Trevor/Desktop/flipped_sub-NDARINV00BD7VDC_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative_recolored.dscalar.nii"
#           )

args <- commandArgs(trailingOnly = TRUE)

ciftiTools.setOption("wb_path", args[1])
eta_LR  <- args[2]
eta_RL  <- args[3]
mask_LR <- args[4]
mask_RL <- args[5]

out <- args[6]

###############################################################################

nets_lut <- tibble(
  net      = 1:16,
  network  = paste0("V", 1:16),
  net_name = c("DMN", "Vis", "FPN", NA, "DAN", NA, "VAN", "Sal", "CO", "SMd",
               "SMI", "AUD", "TP", "MTL", "PMN", "PON")
)

###############################################################################

# Load in mask
cifti_LR <- read_cifti(mask_LR)
cifti_RL <- read_cifti(mask_RL)

# The RL map puts the RH data in the LH, so exclude voxel if it is not in either
# mask
mask_L <- cifti_LR$data$cortex_left | cifti_RL$data$cortex_left

# Replace 0 with NA for later
mask_L_NA <- replace(mask_L, !mask_L, NA)

###############################################################################

# Again extract the data from the origina project and the flipped projection
eta_LR_data <- read_cifti(eta_LR)$data$cortex_left
eta_RL_data <- read_cifti(eta_RL)$data$cortex_left

eta_LmR <- (eta_LR_data - eta_RL_data) * mask_L_NA

saveRDS(eta_LmR, "eta_LmR.rds")

diff_by_net <- apply(eta_LmR, 2, mean, na.rm = TRUE)

###############################################################################

ttest_by_net <- lapply(c(1:3, 5, 7:16), function(.x)
                      t.test(eta_LR_data[, .x] * mask_L_NA[, .x],
                             eta_RL_data[, .x] * mask_L_NA[, .x],
                             paired = TRUE,
                             var.equal = TRUE))

ttest_by_net_tibble <- tibble(
    net   = c(1:3, 5, 7:16),
    ttest = ttest_by_net,
  )

by_net <- left_join(nets_lut, ttest_by_net_tibble) %>%
  mutate(
    diff  = diff_by_net
  )

saveRDS(by_net, out)
