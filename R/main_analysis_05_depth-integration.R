####
# Analysis - Depth Integrated Regression --------------------------------------
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


intg_dat$uvp$Cruise <- factor(intg_dat$uvp$Cruise, 
                              levels = c('jun_night_epi','jun_night_meso',
                                         'jul_day_a','jul_day_b','jul_night'))

# |- Set taxa names -----------------------------------------------------------
taxa_names <- unique(intg_dat$moc$taxa)

####
# Bar-plots ----------------------------------------------------------------
####

bar_plots <- vector('list', length(taxa_names))

intg_plot <- ggplot() +
  geom_bar(data = intg_dat$moc,
           aes(x = taxa, y = intg, fill = Cruise, col = Cruise),
           stat = 'identity', position = position_dodge(width = .9),
           alpha = .25)+
  geom_point(data = intg_dat$uvp[which(intg_dat$uvp$taxa %in% taxa_names),],
             aes(x = taxa, y = mean_conc_m2, col = Cruise),
             position = position_dodge(width = .9),
             size = 2)+
  geom_errorbar(data = intg_dat$uvp[which(intg_dat$uvp$taxa %in% taxa_names),],
                aes(x = taxa, ymin = mean_conc_m2 - sd_conc_m2,
                    ymax = mean_conc_m2 + sd_conc_m2, col = Cruise),
                position = position_dodge(width = .9),
                size = 1, width = .5)+
  labs(x = "", y = "Integrated Abundance [num per square meter]",
       fill = "", col = "")+
  scale_color_manual(values = gg_cbb_col(7)[-c(1:2)])+
  scale_fill_manual(values = gg_cbb_col(7)[-c(1:2)])+
  theme_bw()+
  ab_theme+
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(.9,.9), legend.background = element_blank())


#### 
# # Regressions -----------------------------------------------------------------
####

# # |- Regression Loop ----------------------------------------------------------
# intg_reg <- vector('list', length(taxa_names))
# names(intg_reg) <- taxa_names
# for(i in 1:length(taxa_names)) {
#   uvp_num <- intg_dat$uvp$mean_conc_m2[intg_dat$uvp$taxa == taxa_names[i]]
#   moc_num <- intg_dat$moc$intg[intg_dat$moc$taxa == taxa_names[i]]
#   intg_reg[[i]] <- lm(uvp_num ~ moc_num)
# }
# rm(list = c("uvp_num", "moc_num"))
# 
# # |- Regression Plots ---------------------------------------------------------
# 
# reg_plots <- vector('list', length(taxa_names))
# names(reg_plots) <- taxa_names
# for(i in 1:length(taxa_names)) {
#   uvp_num <- intg_dat$uvp$mean_conc_m2[intg_dat$uvp$taxa == taxa_names[i]]
#   moc_num <- intg_dat$moc$intg[intg_dat$moc$taxa == taxa_names[i]]
#   reg_plots[[i]] <- ggplot() +
#     geom_point(aes(x = moc_num, y = uvp_num)) +
#     geom_abline(slope = 1, intercept = 0,
#                 col = 'grey')+
#     stat_smooth(aes(y = uvp_num, x = moc_num), method = 'lm', se = F)+
#     labs(x = 'Moc', y = 'UVP', subtitle = taxa_names[i])+
#     theme_bw()+
#     ab_theme
# }

####
# Save Data ------------------------------------------------------------------
####
saveRDS(intg_plot, './Output/main_fig_05_integration-comp.rds')
