####
# Management - What is the minimum ESD of a living-identified organism----------
####

# |- Description/Warning ------------------------------------------------------
# note that from the original processing of UVP data already converted
# pixels to mm for ESD & added body volume
# additionally these files already processed the 

# |- Set up & Loading ---------------------------------------------------------
rm(list = ls())
library(EcotaxaTools)
jun_uvp <- readRDS('./Data/uvp_ae1912.RDS')
jul_uvp <- readRDS('./Data/uvp_ae1917.rds')

####
# Data Processing --------------------------------------------------------------
####

# |- Merge Casts into One ------------------------------------------------------
name_map <- list(
  all_casts = c(names(jun_uvp$zoo_files), names(jul_uvp$zoo_files))
)
all_uvp <- merge_casts(c(jun_uvp$zoo_files, jul_uvp$zoo_files), name_map = name_map)[[1]]

# |- Filter to living Only ----------------------------------------------------
l_nl <- names_to(all_uvp, c('living','not-living','darksphere'), suppress_print = T)
all_uvp <- all_uvp[l_nl == 'living',]

####
# Print: what is the min_size? ---------------------------------------------
####
all_uvp[which.min(all_uvp$calc_esd),c('name','calc_esd')]
