library(tidyverse)
library(ciftiTools)
library(moments)

args <- commandArgs(trailingOnly = TRUE)

# args <- c("D:/workbench-windows64-v1.4.2/workbench/bin_windows64",
#           "C:/Users/Trevor/Desktop/7Jg7.dtseries.nii")

wb_path <- args[1]
cifti   <- args[2]
outfile <- args[3]
subid   <- args[4]

##############################################################################

message(wb_path)
ciftiTools.setOption("wb_path", wb_path)
dat <- read_cifti(cifti)

cortex <- rbind(dat$data$cortex_left, dat$data$cortex_right)

##############################################################################

l_skew <- apply(dat$data$cortex_left,  2, skewness)
r_skew <- apply(dat$data$cortex_right, 2, skewness)
t_skew <- apply(cortex, 2, skewness)

l_kurt <- apply(dat$data$cortex_left,  2, kurtosis)
r_kurt <- apply(dat$data$cortex_right, 2, kurtosis)
t_kurt <- apply(cortex, 2, kurtosis)

results <- rbind(l_skew, r_skew, t_skew, l_kurt, r_kurt, t_kurt) %>%
  as_tibble() %>%
  add_column(subid = subid, .before = 1) %>%
  add_column(hemi  = rep(c("l", "r", "t"), 2), .after = 1) %>%
  add_column(var   = rep(c("skewness", "kurtosis"), each = 3), .after = 2)

write_csv(results, outfile)
