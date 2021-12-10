# setwd("G:/My Drive/Research/laterality/abcd-laterality/local_code/")

library(tidyverse)
library(raveio)

# args <- "mats-2633/sub-NDARINV00BD7VDC.mat"
args <- commandArgs(trailingOnly = TRUE)
out <- str_replace(args[1], "mat$", "rds")

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

voxelwise_data <- read_mat(args[1])$eta_to_template_vox
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
