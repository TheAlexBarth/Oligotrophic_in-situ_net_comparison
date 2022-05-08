####
# Analysis - How does bin-width impact depth integrated abundance? -------------
####

# |- Descriptions --------------------------------------------------------------
# Here I'm looking at how bin sizes affect the depth integrated abundance
# Ideally a smaller bin size is more informative, however, it can be biased 

# |- Set up -------------------------------------------------------------------
rm(list = ls())
library(ggplot2)
library(EcotaxaTools)
uvp_list <- list(ae1912 = readRDS('./Data/uvp_ae1912.rds'),
                 ae1917 = readRDS('./Data/uvp_ae1917.rds'))

get_factors <- function(num) {
  num <- round(num, -2)
  x <- c(1:num)
  x <- x[num %% x == 0L]
  return(x)
}

conc_at_width <- function(width, cast, max_d, ecopart_obj) {
  bins <- uvp_conc(ecopart_obj, cast, depth_breaks = seq(0,max_d, width))
  return(bins)
}

integrate_bin_width <- function(ecopart_obj,...) {
  casts <- names(ecopart_obj$zoo_files)
  max_d <- sapply(ecopart_obj$zoo_files,
                    function(x) max(x[['depth_including_offset']]))
  # only keep max depths that go to 1000
  casts <- casts[which(round(max_d, -2) == 1000)]
  out_list <- vector('list', length(casts))
  names(out_list) <- casts
  for(i in 1:length(casts)) {
    widths <- get_factors(max_d[i])
    bin_list <- lapply(widths, conc_at_width,
                            casts[i], max_d[i], ecopart_obj)
    names(bin_list) <- widths
    out_list[[i]] <- try(lapply(bin_list, 
                        function(x) integrate_all(x,need_format = T, ...)|>
                          intg_to_tib()) |> list_to_tib('bin_width'))
  }
  return(out_list)
}

####
# Get Integrated dfs ----------------------------------------------------------
####
ae1912_intg <- integrate_bin_width(uvp_list$ae1912, subdivisions = 1000, rel.tol = 2)
ae1917_intg <- integrate_bin_width(uvp_list$ae1917, subdivisions = 1000, rel.tol = 2)

all_intg <- list_to_tib(c(ae1912_intg, ae1917_intg),
                        'cast')
all_intg <- all_intg[all_intg$taxa != 'detritus' &
                       as.numeric(all_intg$bin_width) <=100,]
intg_bin_plots <- vector('list', length(unique(all_intg$taxa))) 
for(i in 1:length(intg_bin_plots)) {
  intg_bin_plots[[i]] <- ggplot(all_intg[all_intg$taxa == unique(all_intg$taxa)[i],]) +
    geom_line(aes(x = as.numeric(bin_width), y = intg, group = cast),
              alpha = .5) +
    scale_x_continuous(breaks = as.numeric(unique(all_intg$bin_width)),
                       expand = c(0,0))+
    labs(y = paste0(unique(all_intg$taxa)[i], ' Integrated Abundance [num per square m]'),
         x = "Bin Width [m]")+
    theme_bw()
}

###
# Save the data ---------------------------------------------------------------
###
saveRDS(intg_bin_plots,
        './Output/supp_fig_04_bin-width-integration.rds')

