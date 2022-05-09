####
# Analysis - How does abundance estimates look over a depth profile? ----
####


###
# Set up and data loading -------------------------------
###

## |- Delete and read in library -----------------------
rm(list = ls())
library(ggplot2)
source('./R/tools/tool_helper_style-functions-defaults.r')

## |- Load in Data -------------------------------------
moc_conc <- readRDS('./Data/moc_all-binned-densities.rds')
uvp_list <- readRDS('./Data/uvp_all-binned-densities_indp.rds')

taxa_names <- c('Chaetognatha','Copepoda', "Annelida", 'Shrimp-like', 'Ostra/Clado')
profile_names <- c('June_Night','July_Day_A','July_Day_B','July_Night')

###
# Creating Plots -------------------------------------
###
plot_list <- vector('list')
p_index <- 0 #set to assign to the plot list
for(i in 1:length(profile_names)) {
  for(j in 1:length(taxa_names)) {
    p_index <- p_index + 1 #increment 1
    taxa = taxa_names[j]
    plot_list[[p_index]] <- ggplot() +
      geom_rect(data = moc_conc[[i]][moc_conc[[i]]$taxa == taxa,],
                aes(xmin = min_d,xmax = max_d,
                    ymin = 0, ymax = conc_m3,
                    fill = 'MOCNESS', color = 'MOCNESS'),
                alpha = .5)+
      geom_rect(data = uvp_list$pooled[[i]][uvp_list$pooled[[i]]$taxa == taxa,],
                aes(xmin = min_d, xmax = max_d,
                    ymin = 0, ymax = conc_m3,
                    fill = 'Pooled-Cast UVP', color = 'Pooled-Cast UVP'),
                alpha = .5)+
      geom_point(data = uvp_list$avged[[i]][uvp_list$avged[[i]]$taxa == taxa,],
                 aes(x = mp, y = mean, color = 'Avg-Cast UVP'),
                 alpha = 0.5)+
      geom_errorbar(data = uvp_list$avged[[i]][uvp_list$avged[[i]]$taxa == taxa,],
                    aes(x = mp, ymin = mean - sd, ymax = mean + sd,
                        color = 'Avg-Cast UVP'),
                    size = 1, alpha = 0.5)+
      scale_color_manual(values = c(gg_cbb_col(2),'#8a5f00'),
                         breaks = c('MOCNESS','Pooled-Cast UVP', 'Avg-Cast UVP'),
                         limits = 'Avg-Cast UVP')+
      scale_fill_manual(values = gg_cbb_col(2), breaks = c('MOCNESS', 'Pooled-Cast UVP'))+
      scale_x_reverse()+
      coord_flip()+
      labs(x = 'Depth [m]', y = paste0(taxa," [per cubic m]"),
           subtitle = profile_names[i], fill = "", color = "")+
      theme_bw()+
      theme(legend.position = c(0.9,0.1), legend.background = element_blank())
  }
}

####
# Saving the data --------------------------------------------------------------
####
names(plot_list) <- paste(rep(profile_names, each = length(taxa_names)), 
                          taxa_names,sep = '_')
saveRDS(plot_list, './Output/main_fig_03_profile-density-comparison.rds')
