####
# Analysis - Volume sampled on each cast
####
rm(list = ls())
library(EcotaxaTools)
library(ggplot2)

# |- Import uvp ecopart objs ----------------------------------------------------

uvp_list <- list(
  ae1912 = readRDS('./data/uvp_ae1912.rds'),
  ae1917 = readRDS('./data/uvp_ae1917.rds')
)

####
# get volumes -----------------------------------------------------------------
####

ae1912_vol <- uvp_list$ae1912 |> get_ecopart_vol()
ae1917_vol <- uvp_list$ae1917 |> get_ecopart_vol()

# Note that the projects are already trimmed from my other scripts

all_vols <- c(ae1912_vol, ae1917_vol) |> list_to_tib('Cast')
all_vols$db <- cut(all_vols$depth, c(0, 250, 1000))

# |- Get Cast-total & average cast volume --------------------------------------
total_vol_cast <- aggregate(all_vols$vol_sampled, by = list(db = all_vols$db,
                                                            cast = all_vols$Cast),
                            FUN = sum)

avg_vol <- aggregate(list(vol_sampled = all_vols$vol_sampled),
                     by = list(depth = all_vols$depth),
                     FUN = mean)

####
# Plot ----------------------------------------------------------------------
####
vol_plot <- ggplot()+
  geom_line(data = all_vols,
            aes(x = depth, y = vol_sampled, group = Cast),
            col = 'grey', alpha = 0.25)+
  geom_line(data = avg_vol,
            aes(x = depth, y = vol_sampled),
            col = 'black', size = 2)+
  geom_hline(yintercept = floor(1/.035)*1.1, col = 'red')+
  scale_x_reverse()+
  coord_flip()+
  theme_bw()+
  labs(y = 'Volume Sampled [cubic m]', x = 'Depth [m]')

###
# Saves ----------------------------------------------------------------------

saveRDS(vol_plot, './Output/supp_fig_01_vol-sampled.rds')
saveRDS(list(avg = avg_vol,
             cast_sum = total_vol_cast),
  './Output/data_supp_01_voldata.rds')