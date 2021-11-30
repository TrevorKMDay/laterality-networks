#!/bin/bash

module load R/4.1.0

Rscript \
    /home/feczk001/day00096/laterality/code/calculate_mat_stuff_MSI.R \
    "${1}"