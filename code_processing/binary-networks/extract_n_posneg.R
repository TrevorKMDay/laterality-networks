suppressPackageStartupMessages(library(tidyverse))
library(ciftiTools)

# Identify wb_cmd from path
wb_path <- system("dirname $(which wb_command)", intern = TRUE)
if (is.na(wb_path)) {
    quit("Couldn't find wb_command")
} else {
    message(paste("Found wb_command:", wb_path))
    ciftiTools.setOption("wb_path", wb_path)
    message()
}

args  <- commandArgs(trailingOnly = TRUE)
n_max <- args[1]
message(n_max)

################################################################################

message("Identifying list of subtraction maps")
sfiles <- list.files("subtract", pattern = ".*.nii", full.names = TRUE)
message(paste("Found", length(sfiles), "subtracted niftis"))
message()

for (f in sfiles) {

    sub <- str_extract(f, "sub-[A-Z0-9]*")

    print(f)
    dat <- read_cifti(f)$data$cortex_left
    dat_bin <- replace_na(dat / abs(dat), 0)

    n_pos <- apply(dat_bin == 1, 2, sum)
    n_neg <- apply(dat_bin == -1, 2, sum)

    result <- tibble(
            pos = n_pos,
            neg = n_neg
        )

    write_csv(result, paste0(sub, "_subtraction.csv"))
    print(sub)

}