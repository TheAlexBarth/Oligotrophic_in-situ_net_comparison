##
# Tool for renaming mocness
##
#function to switch names
rename <- function(old_name) {
  #Create vectors of name groups
  nl_names <- c('not-living','detritus','artefact','badfocus<artefact','fiber<detritus',
                'bubble','tail<Chaetognatha','part<Crustacea','scale','dead<Copepoda',
                'leg<Crustacea','part<other','detritus chaetognata','tuff','tail<Actinopterygii',
                'light<detritus','molt','part<siphonophorae','tail<Crustacea',
                'tempgrey','aggregates','fiber<plastic')
  mollusca_names <- c('Gymnosomata','Styliola','Limacinidae','Heteropoda','Limacina inflata',
                      'Gastropoda','Creseidae','Limacina bulimoides','Creseis clava','Diacria',
                      'Bivalvia<Mollusca','Creseis conica','Pteropoda','Cavoliniidae','Diacavolinia',
                      'Diacavolinia','Cavolinia','Cuvierina','Mollusca',
                      'Lingula')
  Cope_names <- c('Calanoida','Cyclopoida','Copepoda','Harpacticoida','Pleuromamma',
                  'Copepoda X','Sapphirinidae','Copepdoa','Pleuromamma xiphias',
                  'Microsetella')
  OstrClad_names <- c('Ostracoda','Cladocera','Clacoera')
  Chaeto_names <- c('Chaetognatha','head<Chaetognatha','head<chaetognatha')
  Rhiz_names <- c("Rhizaria",'Phaeodaria','Foraminifera','Acantharea','Coelodendridae')
  Annelida_names <- c('Annelida','Polychaeta')
  gel_names <- c('Cnidaria<Metazoa','Doliolida','Salpida','Salpidae','Siphonophorae',
                 'part<Siphonophorae','Appendicularia','Oikopleura','medusae')
  naup_names <- c('Nauplii<Crustacea','nauplii<Copepoda','like<Copepoda','nauplii<Crustacea')
  euph_names <- c('Euphausiacea','Decapoda','Euphausia','zoea<Decapoda','Luciferidae',
                  'Crustacea','Eumalacostraca','Euphausiacea','zoea<Brachyura')
  other_names <- c('egg<other','Harosa','other<living','Ceratium','Bryozoa',
                   'egg<Actinopterygii','Diatoma','othertocheck',
                   'Cerianthidae<Ceriantharia<Anthozoa')

  if(old_name %in% nl_names) {
    new_name <- 'not-living'
  } else if(old_name %in% mollusca_names) {
    new_name <- 'Mollusca'
  } else if(old_name %in% Cope_names) {
    new_name <- 'Copepoda'
  } else if(old_name %in% OstrClad_names) {
    new_name <- 'Ostra/Clado'
  } else if(old_name %in% Chaeto_names) {
    new_name <- 'Chaetognatha'
  } else if(old_name %in% Rhiz_names) {
    new_name <- 'Rhizaria'
  } else if(old_name %in% Annelida_names) {
    new_name <- 'Annelida'
  } else if(old_name %in% naup_names) {
    new_name <- 'Nauplii'
  } else if(old_name %in% gel_names) {
    new_name <- 'Gelatinous'
  } else if(old_name %in% euph_names) {
    new_name <- 'Shrimp-like'
  } else if(old_name %in% other_names) {
    new_name <- 'Other-living'
  } else {
    new_name <- old_name
  }
  return(new_name)
}


moc_rename <- function(moc_names) {
  new_names <- unlist(lapply(moc_names,rename))
  return(new_names)
}
