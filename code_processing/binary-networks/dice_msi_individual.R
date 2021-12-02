suppressPackageStartupMessages(library(tidyverse))
library(ciftiTools)

# Identify wb_cmd from path
wb_path <- system("dirname $(which wb_command)", intern = TRUE)
if (length(wb_path) == 0) {
  stop("Couldn't find wb_command")
} else {
  message(paste("Found wb_command:", wb_path))
  ciftiTools.setOption("wb_path", wb_path)
  message()
}

args  <- commandArgs(trailingOnly = TRUE)

wb_path <- args[1]
files   <- args[2:3]
csv_out <- args[4]

################################################################################

dice <- function(x, y) {

  union <- x & y
  d <- (2 * sum(union)) / (sum(x) + sum(y))

  results <- tibble(
      "dice" = d,
      "overlap" = sum(union),
      "x" = sum(x),
      "y" = sum(y)
    )

  return(results)

}

generate_table <- function(orig, flip, outfile) {

  # Extract and binarize matrices
  orig_data <- orig$data$cortex_left > 0
  flip_data <- flip$data$cortex_left > 0
  orig_rh <- orig$data$cortex_right > 0
  flip_rh <- flip$data$cortex_right > 0

  ol <- apply(orig_data, 2, sum)
  fl <- apply(flip_data, 2, sum)

  li <- (ol - fl) / (ol + fl)

  n <- ncol(orig_data)

  network_dice <- tibble(
    ix     = 1:n,
    dice   = map(ix, ~dice(orig_data[, .x], flip_data[, .x])),
    dice2  = map(ix, ~dice(orig_rh[, .x], flip_rh[, .x])),
    orig_n = ol,
    flip_n = fl,
    li     = li
  ) %>%
  unnest(c(dice, dice2), names_sep = ".")

  write_csv(network_dice, outfile)

}

################################################################################

flip_file <- files[1]
orig_file <- files[2]

orig_data <- read_cifti(orig_file)
flip_data <- read_cifti(flip_file)

generate_table(orig_data, flip_data, csv_out)

