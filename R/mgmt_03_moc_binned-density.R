####
# Management - Get Mocness into a binned-density file -------------------------
####

##
# Set-up and data load-in -----------------------------------------------------
##

rm(list = ls())
source('./R/tools/tool_moc_conc.R')
library(EcotaxaTools)

moc_list <- readRDS('./Data/moc_all-processed.rds')

###
# Calculating the Densities ----
###

#comp_names
comp_names <- c('Copepoda','Chaetognatha','Annelida','Ostra/Clado','Shrimp-like')

##
# this code is clunky because I haven't built up ecotaxa tools yet to work with zooscan well.
# There is a lot of ugly code on how to get it done all hidden in the conc tools
##

## |- Size trimming mocness ---------------------------------------------------
moc_list <- moc_list |> 
  mod_zoo(function(x) x[x$calc_esd >= 0.934,]) |> 
  mod_zoo(names_keep, comp_names)

## |- Making a list of concentrations -----------------------------------------

# ae1912 has different meta-data and must be dealt with separately
conc_list <- vector('list')
conc_list[[1]] <- moc_conc_ae1912(moc_list[[1]],need_to_bin = T)

# assign depth bins
conc_list[[1]]$db <-  factor(c(
  "(791.6,995.7]",
  "(590.4,791.6]",
  '(348.3,590.4]',
  '(253.7,348.3]',
  '(150.4,253.7]',
  '(48,150.5]',
  '(5.1,48]'
), levels = c(
  "(791.6,995.7]",
  "(590.4,791.6]",
  '(348.3,590.4]',
  '(253.7,348.3]',
  '(150.4,253.7]',
  '(48,150.5]',
  '(5.1,48]'
))

# now for ae1917
for(i in 2:length(moc_list)) {
  conc_list[[i]] <- moc_conc_ae1917(moc_list[[i]])
}
#assgin depth bins
conc_list[[2]]$db <- factor(c(
  "(219.7,270.4]",
  "(168.7,219.7]",
  '(140.1,168.7]',
  '(111.1,140.1]',
  '(80.6,111.1]',
  '(50.7,80.6]',
  '(20,50.7]',
  '(0.8,20]'
), levels = c(
  "(219.7,270.4]",
  "(168.7,219.7]",
  '(140.1,168.7]',
  '(111.1,140.1]',
  '(80.6,111.1]',
  '(50.7,80.6]',
  '(20,50.7]',
  '(0.8,20]'
))

conc_list[[3]]$db <- factor(c(
  "(221.4,260.5]",
  "(180.9,221.4]",
  '(150.6,180.9]',
  '(119.4,150.6]',
  '(89,119.4]',
  '(59,89]',
  '(30.7,59]',
  '(0,30.7]'
), levels =  c(
  "(221.4,260.5]",
  "(180.9,221.4]",
  '(150.6,180.9]',
  '(119.4,150.6]',
  '(89,119.4]',
  '(59,89]',
  '(30.7,59]',
  '(0,30.7]'
))

conc_list[[4]]$db <-factor(c(
  "(220.7,259.8]",
  "(180.8,220.7]",
  '(151,180.8]',
  '(120.8,151]',
  '(89.1,120.8]',
  '(59.7,89.1]',
  '(30.4,59.7]',
  '(0,30.4]'
), levels =  c(
  "(220.7,259.8]",
  "(180.8,220.7]",
  '(151,180.8]',
  '(120.8,151]',
  '(89.1,120.8]',
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

conc_list <- lapply(conc_list, pivot_longer, cols = c('Annelida','Chaetognatha','Copepoda',
                                         'Ostra/Clado','Shrimp-like'),
       names_to = 'taxa', values_to = 'conc_m3')

names(conc_list) <- c('jun_night',
                      'jul_day_a',
                      'jul_day_b',
                      'jul_night')
####
# Saving the Data---------------------------------------------------------------
####
saveRDS(conc_list, './Data/moc_all-binned-densities.rds')