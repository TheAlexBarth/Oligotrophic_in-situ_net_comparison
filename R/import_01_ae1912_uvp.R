####
# Importing ae1912 ------------------------------------------------------------
####

rm(list = ls())
library(readr)
library(EcotaxaTools)
source('./R/tools/tool_uvp_renaming.R')

# |- Raw read-ins -------------------------------------------------------------
# #raw read-ins
dat_path <- "~/Data/BATS_UVP/AE1912/Ecopart_Export" #Where to pull data
ae1912_ecopart <- ecopart_import(dat_path)

####
# Management ------------------------------------------------------------------
####

# |- bring in daytimes --------------------------------------------------------
ae1912_ecopart$meta$timeOfDay <- sapply(ae1912_ecopart$meta$sampledate,
                                        timeOfDay,"09:23:00","23:27:00",1)

# |- drop names ---------------------------------------------------------------
ae1912_ecopart <- mod_zoo(ae1912_ecopart,
                          names_drop,
                          c('duplicate', 'badfocus<artefact',
                            'bubble', 'artefact'))


# |- add the calc_esd, body volume and major axis------------------------------
get_calc_esd <- function(df) {
  calc_esd <- df[['esd']] * df[['pixel_mm']]
  return(calc_esd)
}

ae1912_ecopart <- add_zoo(ae1912_ecopart, get_calc_esd, 'calc_esd')
ae1912_ecopart <- add_zoo(ae1912_ecopart, ellps_vol, 'body_vol')


# |- Trim depth ---------------------------------------------------------------
trim_depth <- function(df) {
  depth_col <- get_col_name(df, 'depth_offset')
  min_max <- min(1000, max(df[[depth_col]]) - 50)
  rdf <- df[which(df[[depth_col]] <= min_max), ]
  return(rdf)
}

ae1912_ecopart <- mod_zoo(ae1912_ecopart, trim_depth)
ae1912_ecopart$par_files <- mod_zoo(ae1912_ecopart$par_files, trim_depth)

####
# Save Ecopart file as a raw for supplement -----------------------------------
#####
saveRDS(ae1912_ecopart, './Data/uvp_ae1912_raw.rds')


# |- Rename files -------------------------------------------------------------
ae1912_ecopart <- add_zoo(ae1912_ecopart, uvp_renamer, 'name')
#####
# Save ------------------------------------------------------------------------
####

saveRDS(ae1912_ecopart, file = './Data/uvp_ae1912.RDS')


