####
# Management - Get Mocness into a binned-biomass file -------------------------
####

##
# Set-up and data load-in -----------------------------------------------------
##

rm(list = ls())
source('./R/tools/tool_moc_conc.R')
library(EcotaxaTools)

moc_list <- readRDS('./Data/moc_all-processed.rds')

###
# Calculating the Densities ----------------------------------------------------
###

#comp_names
comp_names <- c('Copepoda','Chaetognatha','Ostra/Clado','Shrimp-like')

##
# this code is clunky because I haven't built up ecotaxa tools yet to work with zooscan well.
# There is a lot of ugly code on how to get it done all hidden in the conc tools

## |- Size trimming mocness & converting biovolume to drymass ----------------
biomass_conv <- function(taxa) {
  switch(taxa,
         'Copepoda' = return(.055),
         'Chaetognatha' = return(.013),
         'Ostra/Clado' = return(.052),
         'Shrimp-like' = return(.027),
         stop("Taxa Name Not Fit"))
}

bv_dm <- function(df) {
  taxo_name <- get_col_name(df, 'taxo_name')
  taxa <- df[[taxo_name]]
  conv_fact <- sapply(df[[taxo_name]], biomass_conv)
  return(df[['calc_esd']]*conv_fact)
}


moc_list <- moc_list |> 
  mod_zoo(function(x) x[x$calc_esd >= 0.934,]) |> 
  mod_zoo(names_keep, comp_names) |>
  add_zoo(bv_dm, 'dry_mass')

## |- Making a list of concentrations -----------------------------------------

# ae1912 has different meta-data and must be dealt with separately
conc_list <- vector('list')
conc_list[[1]] <- moc_conc_ae1912(moc_list[[1]],need_to_bin = T,
                                  func_col = 'dry_mass', func = sum)

# assign depth bins
conc_list[[1]]$db <-  factor(c(
  "(792,996]",
  "(590,792]",
  '(348,590]',
  '(254,348]',
  '(150,254]',
  '(48,150]',
  '(0,48]'
), levels = c(
  "(792,996]",
  "(590,792]",
  '(348,590]',
  '(254,348]',
  '(150,254]',
  '(48,150]',
  '(0,48]'
))

# now for ae1917
for(i in 2:length(moc_list)) {
  conc_list[[i]] <- moc_conc_ae1917(moc_list[[i]],
                                    func_col = 'dry_mass',
                                    func = sum)
}
#assgin depth bins
conc_list[[2]]$db <- factor(c(
  "(220,270]",
  "(169,220]",
  '(140,169]',
  '(111,140]',
  '(80.6,111]',
  '(50.7,80.6]',
  '(20,50.7]',
  '(0,20]'
), levels = c(
  "(220,270]",
  "(169,220]",
  '(140,169]',
  '(111,140]',
  '(80.6,111]',
  '(50.7,80.6]',
  '(20,50.7]',
  '(0,20]'
))

conc_list[[3]]$db <- factor(c(
  "(221,260]",
  "(181,221]",
  '(151,181]',
  '(119,151]',
  '(89,119]',
  '(59,89]',
  '(30.7,59]',
  '(0,30.7]'
), levels =  c(
  "(221,260]",
  "(181,221]",
  '(151,181]',
  '(119,151]',
  '(89,119]',
  '(59,89]',
  '(30.7,59]',
  '(0,30.7]'
))

conc_list[[4]]$db <-factor(c(
  "(221,260]",
  "(181,221]",
  '(151,181]',
  '(121,151]',
  '(89.1,121]',
  '(59.7,89.1]',
  '(30.4,59.7]',
  '(0,30.4]'
), levels =  c(
  "(221,260]",
  "(181,221]",
  '(151,181]',
  '(121,151]',
  '(89.1,121]',
  '(59.7,89.1]',
  '(30.4,59.7]',
  '(0,30.4]'
))
names(conc_list) <- names(moc_list)

## |- Formatting Output -------------------------------------------------------
for(i in 1:length(conc_list)) {
  bin_format <- get_bin_limtis(conc_list[[i]]$db)
  conc_list[[i]]$min_d <- bin_format$min_d
  conc_list[[i]]$max_d <- bin_format$max_d
  conc_list[[i]]$mp <- bin_format$mp
  rm(bin_format)
}

conc_list <- lapply(conc_list, pivot_longer, cols = c('Chaetognatha','Copepoda',
                                                      'Ostra/Clado','Shrimp-like'),
                    names_to = 'taxa', values_to = 'conc_m3')

names(conc_list) <- c('jun_night',
                      'jul_day_a',
                      'jul_day_b',
                      'jul_night')
####
# Saving the Data---------------------------------------------------------------
####
saveRDS(conc_list, './Data/moc_all-binned-biomass.rds')