####
# Analysis - Depth Integrated Comparison --------------------------------------
####
rm(list = ls())
library(EcotaxaTools)
library(ggplot2)
source('./R/Tools/tool_helper_style-functions-defaults.R')

# |- Load in the Data ---------------------------------------------------------
intg_dat <- readRDS('./Data/integrated_all_density.rds')

# |- Format Cruise factors ----------------------------------------------------
intg_dat$moc$Cruise <- factor(intg_dat$moc$Cruise, 
                              levels = c('jun_night_epi','jun_night_meso',
                                         'jul_day_a','jul_day_b','jul_night'))


intg_dat$avg_uvp$Cruise <- factor(intg_dat$avg_uvp$Cruise, 
                              levels = c('jun_night_epi','jun_night_meso',
                                         'jul_day_a','jul_day_b','jul_night'))


intg_dat$pool_uvp$Cruise <- factor(intg_dat$pool_uvp$Cruise, 
                                  levels = c('jun_night_epi','jun_night_meso',
                                             'jul_day_a','jul_day_b','jul_night'))

# |- Set taxa names -----------------------------------------------------------
taxa_names <- unique(intg_dat$moc$taxa)

####
# Bar-plots ----------------------------------------------------------------
####

avg_intg_plot <- ggplot() +
  geom_bar(data = intg_dat$moc,
           aes(x = taxa, y = intg, fill = Cruise, col = Cruise),
           stat = 'identity', position = position_dodge(width = .9),
           alpha = .25)+
  geom_point(data = intg_dat$avg_uvp[which(intg_dat$avg_uvp$taxa %in% taxa_names),],
             aes(x = taxa, y = mean_conc_m2, col = Cruise),
             position = position_dodge(width = .9),
             size = 3)+
  geom_errorbar(data = intg_dat$avg_uvp[which(intg_dat$avg_uvp$taxa %in% taxa_names),],
                aes(x = taxa, ymin = mean_conc_m2 - sd_conc_m2,
                    ymax = mean_conc_m2 + sd_conc_m2, col = Cruise),
                position = position_dodge(width = .9),
                size = 1, width = .5)+
  labs(x = "", y = bquote(Integrated~Abundace~'[#indv.'~m^-2*']'),
       fill = "", col = "")+
  scale_color_manual(values = gg_cbb_col(7)[-c(1:2)])+
  scale_fill_manual(values = gg_cbb_col(7)[-c(1:2)])+
  theme_bw()+
  ab_theme+
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(.9,.9), legend.background = element_blank())

pool_intg_plot <- ggplot()+
  geom_bar(data = intg_dat$moc,
           aes(x = taxa, y = intg, fill = Cruise, col = Cruise),
           stat = 'identity', position = position_dodge(width = .9),
           alpha = .25)+
  geom_point(data = intg_dat$pool_uvp,
             aes(x = taxa, y = intg, col = Cruise),
             position = position_dodge(width = .9),
             size = 5, shape = 18)+
  labs(x = "", y = bquote(Integrated~Abundace~'[#indv.'~m^-2*']'),
       fill = "", col = "")+
  scale_color_manual(values = gg_cbb_col(7)[-c(1:2)])+
  scale_fill_manual(values = gg_cbb_col(7)[-c(1:2)])+
  theme_bw()+
  ab_theme+
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(.9,.9), legend.background = element_blank())

#### 
# Pair-wise Comparions ---------------------------------------------------------
####

# |- Set-up holder vectors ---------------------------------------------------
avg_moc <- vector('list', length(taxa_names))
pool_moc <- vector('list', length(taxa_names))
avg_pool <- vector('list', length(taxa_names))

wilcox_tester <- function(taxa, df1, df2) {
  tdf <- merge(df1, df2, by = c('Cruise', 'taxa'))
  tdf <- tdf[tdf$taxa == taxa,]
  if(ncol(tdf) == 4){
    test_res <- wilcox.test(tdf$intg.x, tdf$intg.y, paired = T, exact = T)
  } else if(ncol(tdf) == 5) {
    test_res <- wilcox.test(tdf$mean_conc_m2, tdf$intg, paired = T, exact = T)
  } else {
    stop('Something Wrong')
  }
  return(test_res)
}

avg_moc <- lapply(taxa_names, wilcox_tester, intg_dat$avg_uvp, intg_dat$moc)
names(avg_moc) <- taxa_names
pool_moc <- lapply(taxa_names, wilcox_tester, intg_dat$pool_uvp, intg_dat$moc)
names(pool_moc) <- taxa_names
avg_pool <- lapply(taxa_names, wilcox_tester, intg_dat$avg_uvp, intg_dat$pool_uvp)
names(avg_pool) <- taxa_names


# |- Wilcox paired test ignoring taxa ----------------------------------------
all_wilcox_test <- function(df1, df2) {
  tdf <- merge(df1, df2, by = c('Cruise', 'taxa'))
  if(ncol(tdf) == 4){
    test_res <- wilcox.test(tdf$intg.x, tdf$intg.y, paired = T)
  } else if(ncol(tdf) == 5) {
    test_res <- wilcox.test(tdf$mean_conc_m2, tdf$intg, paired = T)
  } else {
    stop('Something Wrong')
  }
  return(test_res)
}

all_avg_moc <- all_wilcox_test(intg_dat$avg_uvp,intg_dat$moc)
all_pool_moc <- all_wilcox_test(intg_dat$pool_uvp, intg_dat$moc)
all_avg_pool <- all_wilcox_test(intg_dat$avg_uvp, intg_dat$pool_uvp)

####
# Save Data ------------------------------------------------------------------
####
saveRDS(list(
  avged = avg_intg_plot,
  pooled = pool_intg_plot
), './Output/main_fig_05_integration-plots.rds')
saveRDS(list(
  avg_moc = avg_moc,
  pool_moc = pool_moc,
  avg_pool = avg_pool
), './Output/data_05_intg_test-res.rds')
