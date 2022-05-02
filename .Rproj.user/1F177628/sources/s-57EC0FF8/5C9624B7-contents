####
# Management: Integrating over depth bins --------------------------------------
####
rm(list = ls())
library(EcotaxaTools)


## |- Load in data-------------------------------------------------------------
moc_conc <- readRDS('./Data/moc_all-binned-densities.rds')
uvp_list <- readRDS('./Data/uvp_all-binned-densities.rds')

####
# Integrating over the MOC-Data------------------------------------------------
####

# |- Splitting AE1912 --------------------------------------------------------
#split the first cruise into eupohotic and deeper
split_deep <- split(moc_conc[[1]],
                    f = cut(moc_conc[[1]]$max_d,c(0,250,1000)))

moc_conc <- c(moc_conc[-1],split_deep)
names(moc_conc)[4:5] <- c('ae1912_epi','ae1912_meso')

# |- Integrating --------------------------------------------------------------
moc_intg <- lapply(moc_conc, 
                   function(x) x |> integrate_all(subdivisions = 1000) |> 
                     intg_to_tib()) |> 
  list_to_tib(new_col_name = 'Cruise')

####
# Integrating UVP Data---------------------------------------------------------
####

# |- Splitting June -----------------------------------------------------------
split_deep <- split(uvp_list[[1]],
                    cut(uvp_list[[1]]$max_d,c(0,250,1000)))
uvp_list <- c(uvp_list[-1],split_deep)
names(uvp_list)[4:5] <- c('ae1912_epi','ae1912_meso')

# |- Integrating --------------------------------------------------------------
uvp_intg <- lapply(uvp_list,
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
intg_out <- list_to_tib(intg_list, 'Device')

saveRDS(intg_out,'./Data/integrated_all_density.rds')