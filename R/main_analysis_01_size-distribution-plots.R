###
# Analysis - size differences in sampling range --------------------------------
###

# |- Data Setup ---------------------------------------------------------------
rm(list = ls())
library(EcotaxaTools)
library(ggplot2)
source("./R/tools/tool_helper_style-functions-defaults.R")
library(cowplot)

#l oad in data
moc_list <- readRDS('./Data/moc_all-processed.rds')
jun_uvp <- readRDS('./Data/uvp_ae1912.rds')
jul_uvp <- readRDS('./Data/uvp_ae1917.rds')

# |- Merge all ----------------------------------------------------------------
moc <- rbind(moc_list[[1]][,which(names(moc_list[[1]]) %in% c('taxo_name',
                                                                   'calc_esd'))],
                  moc_list[[2]][,which(names(moc_list[[2]]) %in% c('taxo_name',
                                                                   'calc_esd'))])

moc_comp <- moc[moc$calc_esd >= 0.934, ] #trim to smallest observed UVP size

uvp_comp <- do.call(rbind, c(jun_uvp$zoo_files, jul_uvp$zoo_files))
uvp_comp <- names_drop(uvp_comp, 'detritus')

# |- Select taxa to compare --------------------------------------------------
comp_taxo <- c('Chaetognatha', 'Annelida', 'Copepoda', 'Shrimp-like',
               'Ostra/Clado')

####
# Creating Plots --------------------------------------------------------------
####

# |- For all MOCNESS RANGE ----------------------------------------------------
size_plots <- vector('list',6)
size_plots[[1]] <- ggplot() +
  geom_density(aes(x = moc$calc_esd, fill = "MOCNESS"), alpha = .5)+
  geom_density(aes(x = uvp_comp$calc_esd, fill = "UVP"), alpha = .5)+
  scale_fill_manual(values = gg_cbb_col(2), breaks = c("MOCNESS","UVP"))+
  labs(x = "ESD [mm]",
       fill = c('MOCNESS', "UVP"), 
       density = "", y = "Num. Individuals",
       subtitle = 'All Living')+
  theme_bw()+
  ab_theme+
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank(),
        legend.title = element_blank())+
  scale_x_continuous(limits = c(0,10))


for(i in 2:(length(comp_taxo)+1)) {
  size_plots[[i]] <- ggplot()+
    geom_density(data = moc[moc$taxo_name == comp_taxo[i-1],],
                 aes(x = calc_esd, fill = 'MOCNESS'),alpha = .5)+
    geom_density(data = uvp_comp[uvp_comp$name == comp_taxo[i-1],],
                 aes(x = calc_esd, fill = 'UVP'), alpha = .5)+
    scale_fill_manual(values = gg_cbb_col(2), breaks = c('MOCNESS','UVP'))+
    labs(x = "ESD [mm]",
         fill = c('MOCNESS', "UVP"), 
         density = "", y = "Num. Individuals",
         subtitle = comp_taxo[i-1])+
    theme_bw()+
    ab_theme+
    theme(legend.position = c(0.9,0.7),
          legend.background = element_blank(),
          legend.title = element_blank())
}

unmatched_size_plots <- plot_grid(size_plots[[1]],
                                  size_plots[[2]],
                                  size_plots[[3]],
                                  size_plots[[4]],
                                  size_plots[[5]],
                                  size_plots[[6]],
                                  nrow = 6,ncol = 1,
                                  align = 'vh')

# |- For Mocness at min uvp size ---------------------------------------------


matched_size_plots <- vector('list',6)
matched_size_plots[[1]] <- ggplot() +
  geom_density(aes(x = moc_comp$calc_esd, fill = "MOCNESS"), alpha = .5)+
  geom_density(aes(x = uvp_comp$calc_esd, fill = "UVP"), alpha = .5)+
  scale_fill_manual(values = gg_cbb_col(2), breaks = c("MOCNESS","UVP"))+
  labs(x = "ESD [mm]",
       fill = c('MOCNESS', "UVP"), 
       density = "", y = "Num. Individuals",
       subtitle = 'All Living')+
  theme_bw()+
  ab_theme+
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank(),
        legend.title = element_blank())+
  scale_x_continuous(limits = c(0,10))


for(i in 2:(length(comp_taxo)+1)) {
  matched_size_plots[[i]] <- ggplot()+
    geom_density(data = moc_comp[moc_comp$taxo_name == comp_taxo[i-1],],
                 aes(x = calc_esd, fill = 'MOCNESS'),alpha = .5)+
    geom_density(data = uvp_comp[uvp_comp$name == comp_taxo[i-1],],
                 aes(x = calc_esd, fill = 'UVP'), alpha = .5)+
    scale_fill_manual(values = gg_cbb_col(2), breaks = c('MOCNESS','UVP'))+
    labs(x = "ESD [mm]",
         fill = c('MOCNESS', "UVP"), 
         density = "", y = "Num. Individuals",
         subtitle = comp_taxo[i])+
    theme_bw()+
    ab_theme+
    theme(legend.position = c(0.9,0.7),
          legend.background = element_blank(),
          legend.title = element_blank())
}

matched_size_list <- list(matched_size_plots[[1]],
                                  matched_size_plots[[2]],
                                  matched_size_plots[[3]],
                                  matched_size_plots[[4]],
                                  matched_size_plots[[5]],
                                  matched_size_plots[[6]])
names(matched_size_list) <- comp_taxo

###
# Save plots ----------------------------
###
saveRDS(unmatched_size_plots, './Output/supp_fig_01_full-size-distribution.rds')
saveRDS(matched_size_list, './Output/main_fig_01_matched-size-distribution.rds')
saveRDS(list(
  moc_full = moc,
  moc_trim = moc_comp,
  uvp = uvp_comp
), './Output/data_01_size-range-dfs.rds')
save(moc_comp, uvp_comp, comp_taxo, file = './Output/data_01_plotting-needs.rda')

