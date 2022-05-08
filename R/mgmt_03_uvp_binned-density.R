####
# Management - getting Uvp to binned density files ----------------------------
####

# |- Description --------------------------------------------------------------
 
# This management file produces two lists of UVP Concentrations for different taxa 
# in discrete depth bins. One of these are 10m wide bins, the other has bins matched 
# to the MOCNESS which are used for regressions in analysis 04. 
# There are two binning methods: One sums uvp casts, this increases the volume
# While the other averages uvp casts.

# |- Loading in the data ------------------------------------------------------
rm(list = ls())
library(EcotaxaTools)

uvp_list <- list(
  ae1912 = readRDS('./Data/uvp_ae1912.rds'),
  ae1917 = readRDS('./Data/uvp_ae1917.rds')
)
####
# Binning summed casts --------------------------------------------------------
####

#trim uvp to just have the taxa of interest
comp_names <- c('Copepoda','Chaetognatha','Annelida','Ostra/Clado','Shrimp-like')
uvp_list$ae1912 <-  mod_zoo(uvp_list$ae1912, names_keep, comp_names)
uvp_list$ae1917 <-  mod_zoo(uvp_list$ae1917, names_keep, comp_names)

# |- Prep for cast merge ------------------------------------------------------

# set up a name map:
ae1912_map <- list(
  night = uvp_list$ae1912$meta$profileid[which(uvp_list$ae1912$meta$timeOfDay == 'night')]
)

ae1917_map <- list(
  day = uvp_list$ae1917$meta$profileid[which(uvp_list$ae1917$meta$timeOfDay == 'day')],
  night = uvp_list$ae1917$meta$profileid[which(uvp_list$ae1917$meta$timeOfDay == 'night')]
)

ae1912_map$night <- ae1912_map$night[which(ae1912_map$night %in% names(uvp_list$ae1912$zoo_files))]
ae1917_map$day <- ae1917_map$day[which(ae1917_map$day %in% names(uvp_list$ae1917$zoo_files))]
ae1917_map$night <- ae1917_map$night[which(ae1917_map$night %in% names(uvp_list$ae1917$zoo_files))]


#|- Independent Binning -------------------------------------------------------

# make a list for concentration storage
indp_depth_breaks <- list(list(db = seq(0,1000,20),
                               ToD = 'night'),
                          list(db = seq(0,270,20),
                               ToD = 'day'),
                          list(db = seq(0,260,20),
                               ToD = 'day'),
                          list(db = seq(0,260,20),
                               ToD = 'night'))

names(indp_depth_breaks) <- c('jun_night',
                              'jul_day_a',
                              'jul_day_b',
                              'jul_night')


indp_conc_list <- vector('list',4)

for(i in 1:length(indp_conc_list)) {
  if(i == 1) {
    indp_conc_list[[i]] <- merge_casts(uvp_list$ae1912, ae1912_map) |> 
      uvp_conc('night', depth_breaks = indp_depth_breaks$jun_night$db) |> 
      bin_format()
  } else if (i > 1) {
    indp_conc_list[[i]] <- merge_casts(uvp_list$ae1917, ae1917_map) |> 
      uvp_conc(cast_name = indp_depth_breaks[[i]]$ToD,
               depth_breaks = indp_depth_breaks[[i]]$db) |> 
      bin_format()
  }
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


names(matched_depth_breaks) <- c('jun_night',
                                 'jul_day_a',
                                 'jul_day_b',
                                 'jul_night')


matched_conc_list <- vector('list',4)

for(i in 1:length(matched_conc_list)) {
  if(i == 1) {
    matched_conc_list[[i]] <- merge_casts(uvp_list$ae1912, ae1912_map) |> 
      uvp_conc('night', depth_breaks = matched_depth_breaks$jun_night$db) |> 
      bin_format()
  } else if (i > 1) {
    matched_conc_list[[i]] <- merge_casts(uvp_list$ae1917, ae1917_map) |> 
      uvp_conc(cast_name = matched_depth_breaks[[i]]$ToD,
               depth_breaks = matched_depth_breaks[[i]]$db) |> 
      bin_format()
  }
}
names(matched_conc_list) <- names(matched_depth_breaks)

####
# Averaging Binned Casts ---------------------------------------------------
####

# |- Matching Function ---------------------------------------------------
cast_assign <- function(cast_name, ecopartobj, db) {
  conc_output <- ecopartobj |> uvp_conc(cast_name, db)
  return(conc_output)
}
avg_casts <- function(casts, ecopartobj, db) {
  conc_list <- lapply(casts, cast_assign, ecopartobj, db)
  conc_df <- do.call(rbind, conc_list)
  mean_df <- aggregate(list(mean = conc_df$conc_m3), by = list(db = conc_df$db,
                                      taxa = conc_df$taxa),
                   FUN = mean) |> add_zeros('mean')
  
  sd_df <- aggregate(list(sd = conc_df$conc_m3), by = list(db = conc_df$db,
                                                taxa = conc_df$taxa),
                     FUN = sd, na.rm = T) |> add_zeros('sd')
  sd_df$sd[is.na(sd_df$sd)] <- 0
  return(structure(merge(mean_df, sd_df), class = c('data.frame', 'etx_conc_obj')))
}

# |- Create Avg Lists --------------------------------------------

indp_avg_list <- list(jun_night = avg_casts(ae1912_map$night,
                                          uvp_list$ae1912,
                                          indp_depth_breaks$jun_night$db),
                      jul_day_a = avg_casts(ae1917_map$day,
                                            uvp_list$ae1917,
                                            indp_depth_breaks$jul_day_a$db),
                      jul_day_b = avg_casts(ae1917_map$day,
                                            uvp_list$ae1917,
                                            indp_depth_breaks$jul_day_b$db),
                      jul_night = avg_casts(ae1917_map$night,
                                            uvp_list$ae1917,
                                            indp_depth_breaks$jul_night$db))
indp_avg_list <- lapply(indp_avg_list, bin_format)

matched_avg_list <- list(jun_night = avg_casts(ae1912_map$night,
                                            uvp_list$ae1912,
                                            matched_depth_breaks$jun_night$db),
                      jul_day_a = avg_casts(ae1917_map$day,
                                            uvp_list$ae1917,
                                            matched_depth_breaks$jul_day_a$db),
                      jul_day_b = avg_casts(ae1917_map$day,
                                            uvp_list$ae1917,
                                            matched_depth_breaks$jul_day_b$db),
                      jul_night = avg_casts(ae1917_map$night,
                                            uvp_list$ae1917,
                                            matched_depth_breaks$jul_night$db))
matched_avg_list <- lapply(matched_avg_list, bin_format)



####
# Save the Data ---------------------------------------------------------------
####
saveRDS(list(pooled = matched_conc_list,
             avged = matched_avg_list),
        './Data/uvp_all-binned-densities_matched.rds')

saveRDS(list(pooled = indp_conc_list,
             avged = indp_avg_list),
        './Data/uvp_all-binned-densities_indp.rds')
