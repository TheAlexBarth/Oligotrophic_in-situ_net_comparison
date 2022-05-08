####
# Analysis - Duplicate's impact on denisty estimates --------------------------
####

# |- Description -------------------------------------------------------------
# this code is an old mess from before I wrote any packages for this code
# I've had to modify it to work with new code but still it is a mess

# |- Set-up -------------------------------------------------------------------
library(ggplot2)
library(ggalt)
library(cowplot)
rm(list = ls())
ae1912 <- readRDS('./Data/uvp_ae1912_raw.rds')

zoo_files <- ae1912$zoo_files
ecopar_vol <- get_ecopart_vol(ae1912)


# |- Get Names ---------------------------------------------------------------
all_names <- do.call(c,lapply(zoo_files,"[[","name"))
tnd_names <- unique(all_names[grep("^tnd<",all_names)]) #names with tnd<
ntnd <- sub(".*<","",unique(all_names[grep("^tnd<",all_names)])) #names without
ntnd <- c(ntnd,"Phaeodaria","Aulacanthidae","Medusettidae","Aulacantha","Eucalanidae",
          "tuff","solitaryblack","solitaryglobule","Coelographis","Castanellidae",
          "puff","colonial<Coelodendridae","Coelodendrum","Aulatractus","colonial<Rhizaria",
          "Aulographis")

####
# Counting with two loops -----------------------------------------------------
####


# |- set-up shells -----------------------------------------------------------
wtnd_list <- vector(mode = "list",length(zoo_files))
wotnd_list <- vector(mode = "list",length(zoo_files))
names(wtnd_list) <- names(zoo_files)
names(wotnd_list) <- names(zoo_files)

# |- Matching Names -----------------------------------------------------------
rhiz_names <- c("Rhizaria","Aulosphaeridae","Acantharea","Castanellidae",
                "Coelodendridae","Collodaria","Foraminifera","Phaeodaria",
                "Aulacanthidae","Medusettidae","Aulacantha","solitaryblack",
                "solitaryglobule","Coelographis","colonial<Coelodendridae",
                "Coelodendrum","Aulatractus","colonial<Rhizaria","Aulographis")

trich_names <- c("puff","tuff")
copep_names <- c("Copepoda","Eucalanidae")

# |- Calculate abundances by 20 sized bin -------------------------------------
for(l in 1:length(zoo_files)){
  df <- zoo_files[[l]] #get specific row
  wtnd <- df[df$name %in% c(tnd_names,ntnd),] #with tnd
  wtnd$name <- sub("tnd<","",wtnd$name) #remove tnd label
  wotnd <- df[df$name %in% c(ntnd),] #without_tnd
  
  #relabel categories
  wtnd$name[which(wtnd$name %in% rhiz_names)] <- "Rhizaria"
  wotnd$name[which(wotnd$name %in% rhiz_names)] <- "Rhizaria"
  wtnd$name[which(wtnd$name %in% trich_names)] <- "Trichodesmium"
  wotnd$name[which(wotnd$name %in% trich_names)] <- "Trichodesmium"
  wtnd$name[which(wtnd$name %in% copep_names)] <- "Copepoda"
  wotnd$name[which(wotnd$name %in% copep_names)] <- "Copepoda"
  
  #calc bins
  abund_wtnd <- bin_taxa(wtnd, seq(0,max(wtnd$depth_including_offset),20))
  abund_wotnd <- bin_taxa(wotnd,seq(0, max(wotnd$depth_including_offset),20))
  
  #assign to lists
  wtnd_list[[l]] <- abund_wtnd
  wotnd_list[[l]] <- abund_wotnd
}


####
# Get Volume Sampled --------------------------------------------------------
####

#calculate volume sampled
# add up the ecopart volume calculations
bin_vol <- vector(mode = "list",length(ecopar_vol))#storage
bin_vol_wo <- vector(mode = "list",length(ecopar_vol)) #stoarge for wotnd
for(l in 1:length(bin_vol)){
  bins <- seq(0,max(ecopar_vol[[l]]$depth)+20,20)
  db <- unlist(lapply(ecopar_vol[[l]]$depth,nearest,bins))
  bin_vol[[l]] <- ecopart_vol_bin(ecopar_vol[[l]],
                                  depth_breaks = seq(0, max(ecopar_vol[[l]]$depth),20))
  names(bin_vol[[l]]) <- c("depth","vol_sampled")
  bin_vol_wo[[l]] <- bin_vol[[l]]
  bin_vol_wo[[l]]$vol_sampled[bin_vol_wo[[l]]$vol_sampled > 35.36977*20] <- 35.36977*20
}

####
# caculate concentraiton ------------------------------------------------------
####
conc_wtnd <- vector(mode = "list",length(wtnd_list))
conc_wotnd <- vector(mode = "list",length(wotnd_list))
for(l in 1:length(conc_wtnd)){
  
  conc_wtnd[[l]] <- merge(wtnd_list[[l]],
                          bin_vol[[l]],
                          by.x = 'db', by.y = 'depth')
  conc_wtnd[[l]]$conc <- conc_wtnd[[l]]$x / conc_wtnd[[l]]$vol_sampled
  conc_wotnd[[l]] <- merge(wotnd_list[[l]],
                           bin_vol_wo[[l]],
                           by.x = 'db', by.y = 'depth')
  conc_wotnd[[l]]$conc <- conc_wotnd[[l]]$x / conc_wotnd[[l]]$vol_sampled
  class(conc_wtnd[[l]]) <- c('etx_conc_obj', 'data.frame')
  class(conc_wotnd[[l]]) <- c('etx_conc_obj', 'data.frame')
  
}
names(conc_wtnd) <- names(zoo_files)
names(conc_wotnd) <- names(zoo_files)
conc_wtnd <- lapply(conc_wtnd, bin_format) |> lapply(add_zeros, 'conc')
conc_wotnd <- lapply(conc_wotnd, bin_format) |> lapply(add_zeros, 'conc')

conc_list <- vector('list',length(conc_wtnd))
for(i in 1:length(conc_wtnd)) {
  conc_list[[i]] <- merge(conc_wtnd[[i]], conc_wotnd[[i]], by = c('db','taxa'),
                          suffixes = c('_with','_removed'))
  class(conc_list[[i]]) <- c('etx_conc_obj', 'data.frame')
  conc_list[[i]] <- bin_format(conc_list[[i]])
}
names(conc_list) <- names(conc_wotnd)

####
# Make Plots -----------------------------------------------------------------
####

dup_plots <- vector('list',length(conc_list) * 5)
taxa_names <- unique(conc_list[[1]]$taxa)
names(dup_plots) <- paste(rep(names(conc_list), each = 5),
                          unique(conc_list[[1]]$taxa),
                          sep = "_")
plot_index <- 0
for(i in 1:length(conc_list)) {
  for(j in 1:length(taxa_names)){
    plot_index <- plot_index + 1
    taxa = taxa_names[j]
    temp_dat <- conc_list[[i]][which(conc_list[[i]]$taxa == taxa),]
    dup_plots[[plot_index]] <- ggplot(temp_dat)+
      geom_dumbbell(aes(y = mp, x=conc_with, xend=conc_removed),
                    color = 'lightgrey', size = .5,
                    colour_x = 'black', size_x = 2,
                    colour_xend = 'blue', size_xend = 2)+
      scale_y_reverse()+
      labs(x = 'Indv/L', y = 'Depth [m]', subtitle = paste(names(conc_list)[i],
                                                           taxa))+
      scale_color_manual(name = c("conc_with",'conc_removed'),
                         values = c('black','blue'))+
      theme_bw()
  }
}

####
# Save the Plots ------------------------------------------------------------
####
saveRDS(dup_plots,
        './Output/supp_fig_03_dup-plots.rds')