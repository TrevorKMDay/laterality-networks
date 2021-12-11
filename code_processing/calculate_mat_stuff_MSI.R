setwd("G:/My Drive/Research/laterality/laterality-wta/code_processing//")

library(tidyverse)
library(raveio)
library(ciftiTools)

args <- c("D:/workbench-windows64-v1.4.2/workbench/bin_windows64",
          "sub-NDARINV00BD7VDC.mat",
          "C:/Users/Trevor/Desktop/sub-NDARINV003RTV85_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative.dtseries.nii",
          "C:/Users/Trevor/Desktop/flipped_sub-NDARINV00BD7VDC_ses-baselineYear1Arm1_task-rest_bold_timeseries_template_matched_Zscored_overlap_smooth_then_derivative_recolored.dscalar.nii"
          )

# args <- commandArgs(trailingOnly = TRUE)

ciftiTools.setOption("wb_path", args[1])
input_mat <- args[2]
mask_LR   <- args[3]
mask_RL   <- args[4]

out <- "./sub-NDARINV00BD7VDC.ods"

###############################################################################

nets_lut <- tibble(
  net      = 1:16,
  network  = paste0("V", 1:16),
  net_name = c("DMN", "Vis", "FPN", NA, "DAN", NA, "VAN", "Sal", "CO", "SMd",
               "SMI", "AUD", "TP", "MTL", "PMN", "PON")
)


m_to_long <- function(dat, threshold = 0) {

  half <- nrow(dat) / 2

  dat <- replace(dat, dat <= threshold, 0)

  ldat <- dat %>%
    as_tibble() %>%
    mutate(
      voxel = row_number(),
      lh    = ifelse(voxel <= half, "lh", "rh")
    ) %>%
    group_by(lh) %>%
    mutate(
      vox_pair = row_number()
    )  %>%
    pivot_longer(-c(voxel, vox_pair, lh), names_to = "network") %>%
    pivot_wider(id_cols = c(vox_pair, network), values_from = value,
                names_from = c(lh)) %>%
    mutate(
      diff = lh - rh
    )

}

###############################################################################

# Load in mask
cifti_LR <- read_cifti(mask_LR)
cifti_RL <- read_cifti(mask_RL)

# Take the original mask and the flipped mask, so that the data in the RL left
# hemisphere is actually the RH mask projected onto the left.
# Calculate the union for both directions.
mask_L <- cifti_LR$data$cortex_left  | cifti_RL$data$cortex_left
mask_R <- cifti_LR$data$cortex_right | cifti_RL$data$cortex_right

# Now create the complete
OR_mask <- rbind(mask_L, mask_R)[, -c(4, 6)] %>%
  replace(!., NA)

################################################################################

voxelwise_data <- read_mat(input_mat)$eta_to_template_vox
vwise_long <- m_to_long(voxelwise_data) %>%
  filter(
    !(network %in% c("V4", "V6"))
  )

vwise_subject <- vwise_long %>%
  group_by(network) %>%
  nest() %>%
  mutate(
    mean_diff = map_dbl(data, ~mean(.x$diff)),
    model     = map(data, ~t.test(.x$lh, .x$rh, paired = TRUE,
                                  var.equal = TRUE))
  ) %>%
  left_join(nets_lut) %>%
  select(network, net, net_name, mean_diff, model)

saveRDS(vwise_subject, out)
