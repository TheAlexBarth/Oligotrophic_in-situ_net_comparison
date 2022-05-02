####
# Management - getting Uvp to binned density files ----------------------------
####

# |- Description --------------------------------------------------------------
 
# This management file produces two lists of UVP Concentrations for different taxa 
# in discrete depth bins. One of these are 10m wide bins, the other has bins matched 
# to the MOCNESS

# |- Loading in the data ------------------------------------------------------
rm(list = ls())
library(EcotaxaTools)

uvp_list <- list(
  ae1912 = readRDS('./Data/uvp_ae1912.rds'),
  ae1917 = readRDS('./Data/uvp_ae1917.rds')
)
####
# Binning ----------------------------------------------------------------------
####

#trim uvp to just have the taxa of interest
comp_names <- c('Copepoda','Chaetognatha','Annelida','Ostra/Clado','Shrimp-like')
uvp_list$ae1912 <-  mod_zoo(uvp_list$ae1912, names_keep, comp_names)
uvp_list$ae1917 <-  mod_zoo(uvp_list$ae1917, names_keep, comp_names)

# |- Prep for cast merge ------------------------------------------------------

# set up a name map:
ae1912_map <- list(
  day = uvp_list$ae1912$meta$profileid[which(uvp_list$ae1912$meta$timeOfDay == 'day')]
)

ae1917_map <- list(
  day = uvp_list$ae1917$meta$profileid[which(uvp_list$ae1917$meta$timeOfDay == 'day')],
  night = uvp_list$ae1917$meta$profileid[which(uvp_list$ae1917$meta$timeOfDay == 'night')]
)

#|- Independing Binning -------------------------------------------------------

# make a list for concentration storage
indp_depth_breaks <- list(list(db = seq(0,1000,10),
                               ToD = 'night'),
                          list(db = seq(0,270,10),
                               ToD = 'day'),
                          list(db = seq(0,260,10),
                               ToD = 'day'),
                          list(db = seq(0,260,10),
                               ToD = 'night'))

names(indp_depth_breaks) <- c('jun',
                              'jul_m14',
                              'jul_m15',
                              'jul_m16')


indp_conc_list <- vector('list',4)

for(i in 1:length(indp_conc_list)) {
  if(i == 1) {
    indp_conc_list[[i]] <- merge_casts(uvp_list$ae1912, ae1912_map) |> 
      uvp_conc('day', depth_breaks = indp_depth_breaks$jun$db)
  } else if (i > 1) {
    indp_conc_list[[i]] <- merge_casts(uvp_list$ae1917, ae1917_map) |> 
      uvp_conc(cast_name = indp_depth_breaks[[i]]$ToD,
               depth_breaks = indp_depth_breaks[[i]]$db)
  }
}

for(i in 1:length(indp_conc_list)){
  info_cols <- get_bin_limtis(indp_conc_list[[i]]$db)
  indp_conc_list[[i]]$min_d <- info_cols$min_d
  indp_conc_list[[i]]$max_d <- info_cols$max_d
  indp_conc_list[[i]]$mp <- info_cols$mp
}
names(indp_conc_list) <- names(indp_depth_breaks)

# |- Matched binning ----------------------------------------------------------

matched_depth_breaks <- list(list(db = c(0,50,150,250,350,600,800,1000),
                          ToD = 'night'),
                     list(db = c(0,20,50,80,110,140,170,220,270),
                          ToD = 'day'),
                     list(db = c(0,30,60,90,120,150,180,220,260),
                          ToD = 'day'),
                     list(db = c(0,30,60,90,120,150,180,220,260),
                          ToD = 'night'))


names(matched_depth_breaks) <- c('jun',
                              'jul_m14',
                              'jul_m15',
                              'jul_m16')


matched_conc_list <- vector('list',4)

for(i in 1:length(matched_conc_list)) {
  if(i == 1) {
    matched_conc_list[[i]] <- merge_casts(uvp_list$ae1912, ae1912_map) |> 
      uvp_conc('day', depth_breaks = matched_depth_breaks$jun$db)
  } else if (i > 1) {
    matched_conc_list[[i]] <- merge_casts(uvp_list$ae1917, ae1917_map) |> 
      uvp_conc(cast_name = matched_depth_breaks[[i]]$ToD,
               depth_breaks = matched_depth_breaks[[i]]$db)
  }
}

for(i in 1:length(matched_conc_list)){
  info_cols <- get_bin_limtis(matched_conc_list[[i]]$db)
  matched_conc_list[[i]]$min_d <- info_cols$min_d
  matched_conc_list[[i]]$max_d <- info_cols$max_d
  matched_conc_list[[i]]$mp <- info_cols$mp
}
names(matched_conc_list) <- names(matched_depth_breaks)

####
# Save the Data ---------------------------------------------------------------
####
saveRDS(matched_conc_list, './Data/uvp_all-binned-densities_matched.rds')
saveRDS(indp_conc_list, './Data/uvp_all-binned-densities_indp.rds')
