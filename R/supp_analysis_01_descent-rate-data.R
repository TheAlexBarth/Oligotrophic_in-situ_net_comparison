####
# Analysis: Importing the data for the descent calcs -------------------------
####

# |- Description --------------------------------------------------------------
# This file imports the data from an external harddrive. The raw files for this 
# are not available on the git project because it would be too large. These files
# are also not available through ecotaxa because it needs the miliseconds of acquisition
# from the Uvp
rm(list = ls())
library(EcotaxaTools)
library(ggplot2)

# |- Import uvp ecopart objs ----------------------------------------------------

uvp_list <- list(
  ae1912 = readRDS('./data/uvp_ae1912.rds'),
  ae1917 = readRDS('./data/uvp_ae1917.rds')
)

# |- Reading and calculating functions ----------------------------------------
# This all needs to be moved into the inside of ecotaxatools
library(lubridate)

dat_file_reader <- function(dfile_name) {
  dfile <- read.table(dfile_name, header = F, sep = ';')
  #get header names from PIQv webiste
  return(dfile)
}

sensor_offset <- function(ecopart_obj) {
  d_offset <- ecopart_obj$meta$acq_depthoffset
  doff <- unique(d_offset)
  if(length(doff) > 1) {
    warning('There are multiple depth_offset values. Check your metadata')
  }
  if(is.na(doff)[1]) {
    return(1.2)
  } else {
    return(doff[1])
  }
}

get_descent_speed <- function(meta_profileid, proj_path, ecopart_obj) {
  meta_index <- which(ecopart_obj$meta$profileid == meta_profileid) #get row index
  file_loc <- paste0(proj_path, '/work/',meta_profileid,'/',meta_profileid,
                     '_datfile.txt') # get file location
  dfile <- dat_file_reader(file_loc) #read dat file
  trim_range <- which(dfile[,1] == nearest(ecopart_obj$meta$firstimage[meta_index], 
                                           dfile[,1])):nrow(dfile)
  depth <- dfile[,3] / 10 + 1.2 # convert dbar to msw and sensor offset
  depth <- depth[trim_range] #trim
  pic_time <- strsplit(dfile[,2], '\t') |> sapply(`[[`,2)
  pic_time <- pic_time[trim_range]
  #calc datetime and ms
  dt <- strsplit(pic_time,'_') |> sapply(`[[`,1)
  ms <- strsplit(pic_time,'_') |> sapply(`[[`,2)
  secs <- as.numeric(seconds(ymd_hms(dt)) + seconds(ms)/1000)
  
  # get index for unique depth entries
  depth_unique <- depth[c(TRUE, depth[-1] != depth[-length(depth)])]
  secs_unique <- secs[c(TRUE, depth[-1] != depth[-length(depth)])]
  
  dr <- diff(depth_unique) / diff(secs_unique)
  
  return(data.frame(
    depth = depth_unique[-1],
    dr = dr
  ))
}

####
# Calculate Rates ------------------------------------------------------------
####

ae1912_rates <- uvp_list$ae1912$meta$profileid |> 
  lapply(get_descent_speed,
         "G:/Processed Projects/uvp5_sn209_bats_ae_1912",
         uvp_list$ae1912)
names(ae1912_rates) <- uvp_list$ae1912$meta$profileid

ae1917_rates <- names(uvp_list$ae1917$zoo_files) |> 
  lapply(get_descent_speed,
         'G:/uvp5_sn209_bats_361',
         uvp_list$ae1917)
names(ae1917_rates) <- names(uvp_list$ae1917$zoo_files) 

# |- Trim to depth zones -----------------------------------------------------
all_rates <- c(ae1912_rates, ae1917_rates) |> list_to_tib()
all_rates <- all_rates[all_rates$depth < 1000,]
# |- Get averages ------------------------------------------------------------
avg_rate <- aggregate(list(mean_dr = all_rates$dr),
                     by = list(depth = all_rates$depth),
                     FUN = mean, na.rm = T)

####
# Plot Rates ------------------------------------------------------------------
####

rates_plot <- ggplot() +
  geom_line(data = all_rates, aes(x = depth, y = dr, group = group),
            col = 'grey', alpha = 0.25)+
  geom_line(data = avg_rate, aes(x = depth, y = mean_dr),
            col = 'black', size = 2)+
  geom_hline(yintercept = 0.7, col = 'red')+
  geom_hline(yintercept = 0, col = 'black')+
  scale_x_reverse()+
  coord_flip()+
  theme_bw()+
  labs(x = 'Depth [m]', y = 'Descent Rate [m/s]')

saveRDS(list(upper = mean(avg_rate$mean_dr[avg_rate$depth < 200]),
             lower = mean(avg_rate$mean_dr[avg_rate$depth > 200])),
        './Output/data_supp_01_descent-averages.rds')
saveRDS(rates_plot, './Output/supp_fig_01_descent-rates.rds')
