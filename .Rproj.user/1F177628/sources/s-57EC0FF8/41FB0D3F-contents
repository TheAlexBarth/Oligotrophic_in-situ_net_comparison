####
# Management - Getting mocness files to comparable format ---------------------
####

# |- Loading in data ----------------------------------------------------------
rm(list = ls())
library(EcotaxaTools)

moc_list <- list(
  ae1912 = readRDS('./Data/moc_ae1912.rds'),
  ae1917 = readRDS('./Data/moc_ae1917.rds')
)

# |- Assigning a body size ----------------------------------------------------

# assign body volume and calc_esd
get_calc_esd <- function(df) {
  calc_esd <- df$esd * unique(df$pixel_mm)
  return(calc_esd)
}

moc_list <- moc_list |>
  add_zoo(get_calc_esd, 'calc_esd') |>
  add_zoo(ellps_vol, 'body_vol')


# |- Random Mapping for mocness duplicates -------------------------------------

set.seed(20211014)
# random sample function
rand_size <- function(df, taxo_name, col_name) {
  size_dist <- df[[col_name]][which(df[['taxo_name']] == taxo_name)]
  return(sample(size_dist, 1))
}

# mapping to apply to dfs - definitely there's a faster way to do this
loop_map <- function(df) {
  for(i in 1:nrow(df)) {
    if(df$morpho_include[i] == T) {
      next()
    } else {
      df$body_vol[i] <- rand_size(df, df$taxo_name[i], 'body_vol')
      df$calc_esd[i] <- rand_size(df, df$taxo_name[i], 'calc_esd')
    }
  }
  return(df)
}

moc_list <- lapply(moc_list, loop_map)


# |- Finalize Data -----------------------------------------------------------

moc_list <- mod_zoo(moc_list, names_drop, 'not-living') #drop all non-living

# make into a clear list
moc_list <- list(jun = moc_list$ae1912, 
                 jul_m14 = split(moc_list$ae1917, f= moc_list$ae1917$sample_towID)[[1]],
                 jul_m15 = split(moc_list$ae1917, f= moc_list$ae1917$sample_towID)[[2]],
                 jul_m16 = split(moc_list$ae1917, f= moc_list$ae1917$sample_towID)[[3]])

####
# Save Mocness Data ---------------------------
####
saveRDS(moc_list, './Data/moc_all-processed.rds')


