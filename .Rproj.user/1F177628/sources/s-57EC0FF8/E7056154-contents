##
# Analyis - what are the percent contritubition of living taxa?
##

#############
# Data Loading
#############

#defaults
rm(list = ls())
library(ggplot2)
library(EcotaxaTools)
source("./R/tools/tool_helper_style-functions-defaults.R")
source('./R/tools/tool_uvp_renaming.R')

# load in the mocness data
moc_list <- readRDS('./Data/moc_all-processed.rds')
# UVP data
jun_uvp <- readRDS("./Data/uvp_ae1912.RDS")
jul_uvp <- readRDS('./Data/uvp_ae1917.RDS')


#some other setup

# need to order taxa names to be in decreasing order
sort_sum <- function(df) {
  rdf <- df
  sum_abundance <- aggregate(df$rel_abundance, by = list(taxa = df$taxa),
                             FUN = sum)
  ranked_taxa <- sum_abundance$taxa[order(sum_abundance$x)]
  
  rdf$taxa <- factor(rdf$taxa, levels = rev(ranked_taxa))
  return(rdf)
}


###########
# Middle Management
###########

##
# Mocness
##

#create a version for comparable sized organisms
drop_big <- function(df) {
  rdf <- df[df$calc_esd >=0.894, ]
  return(rdf)
}

moc_comp <- mod_zoo(moc_list, drop_big)

##
# UVP
##

# Need to sum similar UVP Casts for relative taxa estimation

#creating a name-map for binning
uvp_name_map <- list(
  jun_night = jun_uvp$meta$profileid[jun_uvp$meta$timeOfDay == 'night'],
  jul_day = jul_uvp$meta$profileid[jul_uvp$meta$timeOfDay == 'day'],
  jul_night = jul_uvp$meta$profileid[jul_uvp$meta$timeOfDay == 'night']
)

# merge casts into one
pool_uvp <- merge_casts(c(jun_uvp$zoo_files, jul_uvp$zoo_files),
                        uvp_name_map)

##
# Note that as I developed EcotaxaTools, there's better workflows for this
# but I'm taking it quickly with the data structures as they are
pool_rename <- function(df) {
  new_names <- uvp_renamer(df[['name']])
  rdf <- df
  rdf[['name']] <- new_names
  rdf <- rdf[!(rdf$name %in% c('badfocus<artefact','bubble','detritus',
                               'duplicate')), ]
  return(rdf)
}
pool_uvp <- lapply(pool_uvp, pool_rename)

####
# Calculate the Relative Abuances
####

# calculate the relative abudnace for mocness

rel_moc <- moc_list |> rel_taxa() |> list_to_tib() |> sort_sum()
rel_moc_comp <- moc_comp |> rel_taxa() |> list_to_tib() |> sort_sum()
rel_uvp <- pool_uvp |> rel_taxa() |> list_to_tib() |> sort_sum()

#some additional formatting
rel_moc$group <- factor(rel_moc$group, levels = c('jun', 'jul_m14',
                                                  'jul_m15', 'jul_m16'))
rel_moc_comp$group <- factor(rel_moc_comp$group, levels = c('jun', 'jul_m14',
                                                  'jul_m15', 'jul_m16'))
rel_uvp$group <- factor(rel_uvp$group, levels = c('jun_night','jul_night', 'jul_day'))

# setting up plot colors
plot_cols <- data.frame(taxa = unique(c(rel_moc$taxa,
                                        rel_moc_comp$taxa,
                                        rel_uvp$taxa)),
                        cols = gg_cbb_col(length(unique(c(rel_moc$taxa,
                                                          rel_moc_comp$taxa,
                                                          rel_uvp$taxa)))))


###
# Making the Plots
###

rel_moc_plot <- ggplot(rel_moc)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_moc$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

rel_moc_comp_plot <- ggplot(rel_moc_comp)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_moc_comp$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

rel_uvp_plot <- ggplot(rel_uvp)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_uvp$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

saveRDS(list(
  moc = rel_moc_plot,
  moc_comp = rel_moc_comp_plot,
  uvp = rel_uvp_plot
),'./Output/main_analysis02_rel-contribution.rds')