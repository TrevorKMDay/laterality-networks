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
outdir <- args[1]
n_max <- args[2]
message(n_max)

stopifnot(length(args) >= 1)

# wb_path <- args[1]
# files   <- args[2:3]
# csv_out <- args[4]

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

message("Identifying list of flip maps")
flip_files <- list.files("flipped", pattern = "flipped.*.nii",
                          full.names = TRUE)

message(paste("Found", length(flip_files), "flipped niftis"))
message()

sub_ids <- flip_files %>%
  str_extract("sub-[A-Z0-9]*") %>%
  str_remove("sub-")

################################################################################

if (is.na(n_max)){
  n_max <- length(sub_ids)
}


for (i in sub_ids[1:n_max]) {

  outfile <- paste0(outdir, "/sub-", i, ".csv")

  if (!file.exists(outfile)) {

    message(paste("Working on", i))

    flip_file <- flip_files[str_detect(flip_files, i)]
    orig_file <- list.files("source", pattern = paste0("sub-", i, ".*.nii"),
                            full.names = TRUE)

    # message(orig_file)
    # message(flip_file)

    orig_data <- read_cifti(orig_file)
    flip_data <- read_cifti(flip_file)

    # str(orig_data)

    generate_table(orig_data, flip_data, outfile)

  } else {
    message(paste("Skipping", i))
  }

}
cat("", fill = TRUE)

quit()
