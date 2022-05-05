####
# Analyis - what are the percent contritubition of living taxa? --------
####

####
# Data Loading ------------------------
####

# |- Set-up ------------------------
rm(list = ls())
library(ggplot2)
library(EcotaxaTools)
source("./R/tools/tool_helper_style-functions-defaults.R")
source('./R/tools/tool_uvp_renaming.R')

# |- Load in the mocness data--------------
moc_list <- readRDS('./Data/moc_all-processed.rds')

# |- UVP data --------------------------
jun_uvp <- readRDS("./Data/uvp_ae1912.RDS")
jul_uvp <- readRDS('./Data/uvp_ae1917.RDS')


#some other setup

# |- Taxa Name Order Function -------------------------
sort_sum <- function(df) {
  rdf <- df
  sum_abundance <- aggregate(df$rel_abundance, by = list(taxa = df$taxa),
                             FUN = sum)
  ranked_taxa <- sum_abundance$taxa[order(sum_abundance$x)]
  
  rdf$taxa <- factor(rdf$taxa, levels = rev(ranked_taxa))
  return(rdf)
}


####
# Middle Management ----------------------------
####



# |- Mocness Dropping Sizes -----------------
drop_big <- function(df) {
  rdf <- df[df$calc_esd >=0.894, ]
  return(rdf)
}
moc_comp <- mod_zoo(moc_list, drop_big)


# |- UVP Pooling -------------------------------------
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
# Calculate the Relative Abuances ----------------------------
####

# |- Calculations --------------------------------------------
rel_moc <- moc_list |> rel_taxa() |> list_to_tib() |> sort_sum()
rel_moc_comp <- moc_comp |> rel_taxa() |> list_to_tib() |> sort_sum()
rel_uvp <- pool_uvp |> rel_taxa() |> list_to_tib() |> sort_sum()

# |- Reformatting ---------------------------------------------
rel_moc$group <- factor(rel_moc$group, levels = c('jun', 'jul_m14',
                                                  'jul_m15', 'jul_m16'))
rel_moc_comp$group <- factor(rel_moc_comp$group, levels = c('jun', 'jul_m14',
                                                  'jul_m15', 'jul_m16'))
rel_uvp$group <- factor(rel_uvp$group, levels = c('jun_night','jul_night', 'jul_day'))


###
# Relative Abundance across all tows/casts -----------------
###

# |- description ------------------
# Above, the data are separated out by casts, but if we want to know
# what the relative abundance across all casts, we need to pool

# |- Mocness ------------------
moc_map <- list(
  'all' = names(moc_comp)
)
all_moc <- data.frame(name = do.call(c,list(moc_comp[[1]][[get_col_name(moc_comp[[1]],'taxo_name')]],
                   moc_comp[[2]][[get_col_name(moc_comp[[2]],'taxo_name')]],
                   moc_comp[[3]][[get_col_name(moc_comp[[3]],'taxo_name')]],
                   moc_comp[[4]][[get_col_name(moc_comp[[4]],'taxo_name')]]))) |> 
  rel_taxa()

# |- UVP ----------------------
all_uvp_map <- list(
  'all' = c(names(jun_uvp$zoo_files),names(jul_uvp$zoo_files))
)

all_uvp <- merge_casts(c(jun_uvp$zoo_files,jul_uvp$zoo_files), all_uvp_map)[[1]] |>
  pool_rename() |> 
  rel_taxa()

###
# Making the Plots ----------------------------------
###

# |- Plot Colors -------------------------
plot_cols <- data.frame(taxa = unique(c(rel_moc$taxa,
                                        rel_moc_comp$taxa,
                                        rel_uvp$taxa)),
                        cols = gg_cbb_col(length(unique(c(rel_moc$taxa,
                                                          rel_moc_comp$taxa,
                                                          rel_uvp$taxa)))))

# |- Moc not size filtered -------------------------------
rel_moc_plot <- ggplot(rel_moc)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_moc$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

# |- Moc Size Filtered -----------------------------------
rel_moc_comp_plot <- ggplot(rel_moc_comp)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_moc_comp$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

# |- UVP Plot ------------------------------------------
rel_uvp_plot <- ggplot(rel_uvp)+
  geom_bar(aes(x = group,y = rel_abundance, fill = taxa),
           position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot_cols$cols[match(levels(rel_uvp$taxa),
                                                  plot_cols$taxa)])+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",fill = "",y = "Relative Contribution")+
  theme_bw()+
  ab_theme

####
# Saving Data --------------
####
saveRDS(list(
  moc = rel_moc_plot,
  moc_comp = rel_moc_comp_plot,
  uvp = rel_uvp_plot
),'./Output/main_fig_02_rel-contribution.rds')
saveRDS(list(
  moc_unfiltered = rel_moc,
  moc_filtered = rel_moc_comp,
  uvp = rel_uvp,
  moc_filtered_all = all_moc,
  uvp_all = all_uvp
), './Output/data_02_rel-contrib.rds')