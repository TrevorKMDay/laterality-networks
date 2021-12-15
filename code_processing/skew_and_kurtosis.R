library(tidyverse)
library(ciftiTools)

ciftiTools.setOption("wb_path", args[1])

dat <- read_cifti(args[2])

