####
# Management: Integrating over depth bins --------------------------------------
####
rm(list = ls())
library(EcotaxaTools)


## |- Load in data-------------------------------------------------------------
moc_conc <- readRDS('./Data/moc_all-binned-densities.rds')
uvp_list <- list(
  ae1912 = readRDS('./Data/uvp_ae1912.rds'),
  ae1917 = readRDS('./Data/uvp_ae1917.rds')
)

####
# Binning Raw UVP Data --------------------------------------------------------
####

# |- Bin set-ups --------------------------------------------------------------

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


# make a list for concentration storage
indp_depth_breaks <- list(list(db = seq(0,1000,10),
                               ToD = 'night'),
                          list(db = seq(0,270,10),
                               ToD = 'day'),
                          list(db = seq(0,260,10),
                               ToD = 'day'),
                          list(db = seq(0,260,10),
                               ToD = 'night'))

names(indp_depth_breaks) <- c('jun_night',
                              'jul_day_a',
                              'jul_day_b',
                              'jul_night')


indp_conc_list <- vector('list',4)
names(indp_conc_list) <- names(indp_depth_breaks)

cast_assign <- function(cast_name, ecopartobj, db) {
  conc_output <- ecopartobj |> uvp_conc(cast_name, db)
  return(conc_output)
}

# |- Binning Loop -------------------------------------------------------------
for(i in 1:length(indp_conc_list)) {
  #set up special for ae1912
  if(i == 1) {
    indp_conc_list[[i]] <- lapply(ae1912_map$night, cast_assign, uvp_list$ae1912,
                                  indp_depth_breaks$jun_night$db)
    indp_conc_list[[i]] <- lapply(indp_conc_list[[i]], bin_format)
    names(indp_conc_list[[i]]) <- ae1912_map$night
  } else {
    indp_conc_list[[i]] <- lapply(ae1917_map[[indp_depth_breaks[[i]][['ToD']]]],
                                  cast_assign, uvp_list$ae1917,
                                  indp_depth_breaks[[i]]$db)
    indp_conc_list[[i]] <- lapply(indp_conc_list[[i]], bin_format)
    names(indp_conc_list[[i]]) <- ae1917_map[[indp_depth_breaks[[i]][['ToD']]]]
  }
}

####
# Integrating UVP Data---------------------------------------------------------
####

# |- Splitting June -----------------------------------------------------------

# Splitting into a shallow and deep sections
deep_cut <- function(df) {
  r_list <- split(df, cut(df$max_d, c(0,250,1000)))
  return(r_list)
}
cut_june <- lapply(indp_conc_list$jun_night, deep_cut)
jun_night_epi <- lapply(cut_june, `[[`, 1)
jun_night_meso <- lapply(cut_june[-1], `[[`, 2) #remove first cast which only goes 450m

indp_conc_list <- c(list(jun_night_epi = jun_night_epi,
                                         jun_night_meso = jun_night_meso),
                    indp_conc_list[-1])

# |- Integrating --------------------------------------------------------------

avg_integrate <- function(list, ...) {
 temp_intg_list <- lapply(list, integrate_all, ...)
 df_list <- lapply(temp_intg_list, intg_to_tib)
 all_casts <- list_to_tib(df_list)
 rdf <- aggregate(list(mean_conc_m2 = all_casts$intg),
                   by = list(taxa = all_casts$taxa),
                   FUN = mean)
 rdf$sd_conc_m2 <- aggregate(all_casts$intg,
                             by = list(all_casts$taxa),
                             FUN = sd)$x
 return(rdf)
}

uvp_intg <- lapply(indp_conc_list, avg_integrate, rel.tol = 1.5) |> 
  list_to_tib('Cruise')

####
# Integrating over the MOC-Data------------------------------------------------
####

# |- Splitting AE1912 --------------------------------------------------------
#split the first cruise into eupohotic and deeper
split_deep <- split(moc_conc[[1]],
                    f = cut(moc_conc[[1]]$max_d,c(0,250,1000)))

moc_conc <- c(split_deep,moc_conc[-1])
names(moc_conc)[1:2] <- c('jun_night_epi','jun_night_meso')

# |- Integrating --------------------------------------------------------------
moc_intg <- lapply(moc_conc, 
                   function(x) x |> integrate_all(subdivisions = 1000) |> 
                     intg_to_tib()) |> 
  list_to_tib(new_col_name = 'Cruise')

####
# Combine and Save ------------------------------------------------------------
####
intg_list <- list(
  moc = moc_intg,
  uvp = uvp_intg
)

saveRDS(intg_list,'./Data/integrated_all_density.rds')
