###
# Management - Getting Integrated densities
###

rm(list = ls())
library(EcotaxaTools)

uvp_data <- readRDS('./Data/uvp_all-binned-densities.rds')
moc_data <- readRDS('./Data/moc_all-binned-densities.rds')

split_uvp_deep <- split(uvp_data$jun, cut(uvp_data$jun$max_d,
                                          c(0,250,1000)))
uvp_data <- c(uvp_data[-1],split_uvp_deep)


#' Integrate all objects over a bin
integrate_all <- function(df, formatted == F, ...) {
  if(!formatted) {
    info_cols <- get_bin_limtis(df$db)
    df$min_d <- info_cols$min_d
    df$max_d <- info_cols$max_d
    df$mp <- info_cols$mp
  }
  
}