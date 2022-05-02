####
# Analysis - Depth Integrated Regression --------------------------------------
####
rm(list = ls())
library(EcotaxaTools)
library(tidyr)

# |- Load in the Data ---------------------------------------------------------
intg_dat <- readRDS('./Data/integrated_all_density.rds')

#### 
# Regressions -----------------------------------------------------------------
####

# split to individual taxa
intg_by_taxa <- split(intg_dat, intg_dat$taxa)

intg_by_taxa <- lapply(intg_by_taxa, 
                       function(x) pivot_wider(x,names_from = Device, 
                                                 values_from = intg))

# |- Regression List ----------------------------------------------------------
reg_list <- lapply(intg_by_taxa, function(x) lm(x$uvp ~ x$moc) |> summary())
