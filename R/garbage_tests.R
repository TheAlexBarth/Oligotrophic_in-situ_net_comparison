fixed_conc <- function (ecopart_obj, cast_name, depth_breaks){
  if (!(cast_name %in% names(ecopart_obj$zoo_list) | !(cast_name %in% 
                                                       names(ecopart_obj$par_list)))) {
    stop("Error in Cast Name")
  }
  counts <- bin_taxa(ecopart_obj$zoo_files[[cast_name]], depth_breaks = depth_breaks)#, 
                     #...)
  vol_df <- as_tibble(ecopart_vol_bin(get_ecopart_vol(ecopart_obj)[[cast_name]], 
                                      depth_breaks = depth_breaks))
  temp_merge <- merge(counts, vol_df)
  temp_merge$conc_m3 <- (temp_merge$x/temp_merge$vol_sampled) * 
    1000
  rdf <- add_zeros(temp_merge, "conc_m3")
  class(rdf) <- c("data.frame", "etx_conc_obj")
  return(rdf)
}

add_count_zeros <- function(tdf, taxa_names){
  if(all(taxa_names %in% tdf$taxa)) {
    return(tdf)
  } else {
    add_taxa <- taxa_names[which(!(taxa_names %in% tdf$taxa))]
    rdf <- data.frame(taxa = c(tdf$taxa, add_taxa),
                      x = c(tdf$x, rep(0,length(add_taxa))))
    return(rdf)
  }
}

find_uvp_zeros <- function(counts, vol_df) {
  count_bins <- split(counts[,2:3],f = counts$db)
  count_bins <- count_bins[which(names(count_bins) %in% vol_df$db)]
  
}
  
uvp_conc_add_zeros{
  taxa_names <- unique(df$taxa)
  rdf <- data.frame(db = df$db, taxa = df$taxa, ret_col = df[[col_name]])
  for (i in 1:length(unique(df$db))) {
    tdf <- df[df$db == unique(df$db)[i], ]
    add_taxa <- taxa_names[which(!(taxa_names %in% tdf$taxa))]
    if (length(add_taxa) == 0) {
      (next)()
    }
    ndf <- data.frame(db = rep(unique(df$db)[i], length(add_taxa)), 
                      taxa = add_taxa, ret_col = rep(0, length(add_taxa)))
    rdf <- rbind(rdf, ndf)
  }
  rdf <- rdf[order(rdf$db), ]
  names(rdf)[3] <- col_name
  return(rdf)
}