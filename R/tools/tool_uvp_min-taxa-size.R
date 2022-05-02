###
# Tool to get minimum size by taxa for all UVP data
###

min_uvp_size <- function(taxa) {
  jun <- readRDS('./data/uvp_ae1912_zoo_all-casts-raw.rds')
  jul <- readRDS('./data/uvp_ae1917_zoo_all-casts-raw.rds')

  all_uvp <- rbind(do.call(rbind,jun),do.call(rbind,jul))
  min_size <- min(all_uvp$calc_esd[all_uvp$name == taxa])
  return(min_size)
}
