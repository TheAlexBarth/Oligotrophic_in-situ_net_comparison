library(tidyr)

##
# Cacluate concentration of mocness collected zooplankton
moc_conc_ae1912 <- function(moc_data, need_to_bin = T,...){
  source('./R/tools/item_mocness_scan_index.R')
  net_vols <- readRDS("./Data/moc_ae1912_net-vols.rds")

  if(need_to_bin == T) {
  #load in objects
  count_data <- bin_taxa(moc_data,zooscan = T,...)
  count_data <- count_data |> pivot_wider(id_cols = db, names_from = taxa,
                                          values_from = x)
  } else {
    count_data  <- moc_data
  }

  #create a shell to add concentration calculations to
  conc_df <- as.data.frame(matrix(nrow = length(mocness_scan_index),
                                     ncol = ncol(count_data)-1))
  names(conc_df) <- names(count_data[2:ncol(count_data)])
  conc_df$net_bin <- names(mocness_scan_index)

  #for all nets not 2, loop through and calculate the concentration
  for(i in 1:length(mocness_scan_index)){
    if(names(mocness_scan_index)[i] %in% c("two_lg","two_sm")){
      next()
    }
    row_dex <- which(count_data$db %in% mocness_scan_index[[i]]$scans) #get scan index
    total_scan <- apply(count_data[row_dex,2:(ncol(count_data))],2,sum) #add up all summed scans
    which_vol <- net_vols$vol[which(net_vols$net == mocness_scan_index[[i]]$net)] #get net volume

    undilute <- (total_scan / mocness_scan_index[[i]]$dilution) #undilute the sample
    conc_df[i,1:(ncol(count_data)-1)] <-  undilute / net_vols$vol[which(net_vols$net == mocness_scan_index[[i]]$net)] #calculate indv. / cuM
  }

  #for net two, we need to calculate the concentration differently

  #large net was split and diluted but not net three
  row_dex <- which(count_data$db %in% mocness_scan_index[['two_lg']]$scans[c(1,2)]) #net two
  undilute_two <- (apply(count_data[row_dex,2:(ncol(count_data))],
                         2,sum) / mocness_scan_index[['two_lg']]$dilution[1]) #net two
  row_dex <- which(count_data$db %in% mocness_scan_index[['two_lg']]$scans[3]) #net three
  undilute_three <- (apply(count_data[row_dex,2:(ncol(count_data))],
                           2,sum) / mocness_scan_index[['two_lg']]$dilution[2])
  conc_df[which(conc_df$net_bin == 'two_lg'),-ncol(conc_df)] <- (undilute_two + undilute_three) /
    net_vols$vol[which(net_vols$net == mocness_scan_index[['two_lg']]$net)]

  row_dex <- which(count_data$db %in% mocness_scan_index[['two_sm']]$scans[c(1,2)]) #net two
  undilute_two <- (apply(count_data[row_dex,2:(ncol(count_data))],
                         2,sum) / mocness_scan_index[['two_sm']]$dilution[1]) #net two
  row_dex <- which(count_data$db %in% mocness_scan_index[['two_sm']]$scans[3]) #net three
  undilute_three <- (apply(count_data[row_dex,2:(ncol(count_data))],
                           2,sum) / mocness_scan_index[['two_sm']]$dilution[2])

  conc_df[which(conc_df$net_bin == 'two_sm'),-ncol(conc_df)] <- (undilute_two + undilute_three) /
    net_vols$vol[which(net_vols$net == mocness_scan_index[['two_sm']]$net)]

  # merge all rows from the same net:

  net_names <- sub("_[a-z][a-z]","",conc_df$net_bin) #get net names
  out_df <- as.data.frame(matrix(nrow = length(unique(net_names)),
                                 ncol = ncol(conc_df)-1)) #make shell
  names(out_df) <- names(conc_df)[-ncol(conc_df)]
  for(i in 1:nrow(out_df)) {
    conc_dex <- which(net_names %in% unique(net_names)[i])
    out_df[i,] <- apply(conc_df[conc_dex,-ncol(conc_df)],2,sum, na.rm = T)
  }
  out_df$net_bin <- unique(net_names)
  return(out_df)
}

moc_conc_ae1917 <- function(moc_data, need_to_bin = T,...) {
  net_vols <- readRDS('./Data/moc_ae1917_net-vols.rds')
  if(need_to_bin == T) {
    #load in objects
    count_data <- bin_taxa(moc_data,zooscan = T,...)
    count_data <- count_data |> pivot_wider(id_cols = db, names_from = taxa,
                                            values_from = x)
  } else {
    count_data  <- moc_data
    stop('this will not work because I am bad at code, set need to bin to true')
  }

  conc_df <- as.data.frame(matrix(nrow = nrow(count_data),
                                  ncol = ncol(count_data)-1))
  names(conc_df) <- names(count_data)[-1]

  for(i in 1:nrow(count_data)) {
    matcher <- which(net_vols$scan_id == count_data$db[i])
    count_row <- count_data[i,-1]
    mult_row <- count_row * net_vols$dil_id[matcher]
    conc_row <- mult_row / net_vols$vol_match[matcher]
    conc_df[i,] <- conc_row
  }
  nets <- net_vols$net_match[which(net_vols$scan_id %in% moc_data$sample_id)]
  out_df <- aggregate(conc_df, by = list(net = nets), FUN = sum, na.rm = T)
  return(out_df)
}
