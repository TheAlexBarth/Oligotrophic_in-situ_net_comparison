####
# Analysis - Regression for matching depth bins -------------
####

# |- Description --------------------------------------------
# This script takes matching depth bin density estimates between the UVP and MOCNESS
# and attempts to assess a linear relationship between them. This is done for both the pooled-cast
# and avg-cast UVP methods

# |- Data Loading -------------------------------------------
rm(list = ls())
library(ggplot2)
library(EcotaxaTools)
source('./R/tools/tool_helper_style-functions-defaults.R')

moc_conc <- readRDS('./Data/moc_all-binned-densities.rds')

names(moc_conc$jun_night)[which(names(moc_conc$jun_night) == 'net_bin')] <- 'net'

uvp_list <- readRDS('./Data/uvp_all-binned-densities_matched.rds')

####
# Data Oraganization ----------------------------------------
####

# quick function to organize data
mush_frames <- function(uvp_list, moc_list) {
  rdf_uvp <- list_to_tib(uvp_list)
  rdf_moc <- list_to_tib(moc_list)
  
  names(rdf_uvp)[which(names(rdf_uvp) %in% c('conc_m3',
                                             'mean'))] <- 'conc_m3_uvp'
  names(rdf_moc)[which(names(rdf_moc) == 'conc_m3')] <- 'conc_m3_moc'
  
  rdf <- merge(rdf_uvp, rdf_moc, by  = c('min_d','max_d','mp','group','db',
                                         'taxa'))
  return(rdf)
}

pooled_all <- mush_frames(uvp_list$pooled, moc_conc)
avg_all <- mush_frames(uvp_list$avged, moc_conc)

# |- Names of taxa to loop over --------------------------------------
taxa_names <- unique(pooled_all$taxa)

####
# Run Regressions -------------------------------------------------------------
####
pool_regressions <- vector('list', length(taxa_names))
avg_regressions <- vector('list', length(taxa_names))

for(i in 1:length(taxa_names)) {
  pool_regressions[[i]] <- lm(conc_m3_uvp ~ conc_m3_moc,
                              data = pooled_all[pooled_all$taxa == taxa_names[i],])
  avg_regressions[[i]] <- lm(conc_m3_uvp ~ conc_m3_moc,
                             data = avg_all[avg_all$taxa == taxa_names[i],])
}

names(pool_regressions) <- taxa_names
names(avg_regressions) <- taxa_names

####
# Plot Regressions -------------------------------------------------------------
####

pool_plot <- vector('list', length(taxa_names))
avg_plot <- vector('list', length(taxa_names))

names(pool_plot) <- taxa_names
names(avg_plot) <- taxa_names

for(i in 1:length(taxa_names)) {
  taxa = taxa_names[i]
  pool_plot[[i]] <- ggplot(pooled_all[pooled_all$taxa == taxa,])+
    geom_point(aes(x = conc_m3_moc, y = conc_m3_uvp, col = group),
               size = 2) +
    geom_abline(slope = 1, intercept = 0,
                size = 1, col = gg_cbb_col(2)[1],
                linetype = 'dashed')+
    stat_smooth(aes(x = conc_m3_moc, y = conc_m3_uvp),
                method = 'lm',
                size = 1, se = F, col = 'black')+
    scale_y_continuous(expand = expansion(c(0,.1)))+
    scale_x_continuous(expand = expansion(c(0,0.1)))+
    labs(x = bquote(MOCNESS~'[#indv.'~m^-3*']'),y = bquote(UVP~'[#indv.'~m^-3*']'), subtitle = taxa,
         col = '')+
    scale_color_manual(values = gg_cbb_col(length(unique(pooled_all$group))))+
    theme_bw()+
    ab_theme +
    theme(legend.position = c(0.85,0.85))
  
  avg_plot[[i]] <- ggplot(avg_all[avg_all$taxa == taxa,])+
    geom_point(aes(x = conc_m3_moc, y = conc_m3_uvp, col = group),
               size = 2) +
    geom_abline(slope = 1, intercept = 0,
                size = 1, col = gg_cbb_col(2)[1],
                linetype = 'dashed')+
    stat_smooth(aes(x = conc_m3_moc, y = conc_m3_uvp),
                method = 'lm',
                size = 1, se = F, col = 'black')+
    scale_y_continuous(expand = expansion(c(0,.1)))+
    scale_x_continuous(expand = expansion(c(0,0.1)))+
    labs(x = bquote(MOCNESS~'[#indv.'~m^-3*']'),y = bquote(UVP~'[#indv.'~m^-3*']'), subtitle = taxa)+
    scale_color_manual(values = gg_cbb_col(length(unique(avg_all$group))))+
    theme_bw()+
    ab_theme+
    theme(legend.position = c(0.85,0.85))
  
}

####
# Save the Data ---------------------------------------------------------------
####
saveRDS(list(
  pooled = pool_plot,
  avged = avg_plot
), './Output/main_fig_04_regression-plots.rds')
saveRDS(list(
  pooled = pool_regressions,
  avged = avg_regressions
), './Output/data_04_regressions-models.rds')

