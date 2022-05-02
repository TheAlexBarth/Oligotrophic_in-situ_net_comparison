##
# UVP name manager
##

#' uvp_renamer -
name_selector <- function(obj_name) {
  rhiz_names <- c("Rhizaria","Foraminifera","Phaeodaria","Collodaria","Aulacanthidae",
                  "Coelodendridae",'Medusettidae','Aulosphaeridae','Aulacantha',
                  'solitaryblack','solitaryglobule','Coelographis','Castanellidae',
                  'tnd<Collodaria','tnd<Castanellidae','colonial<Coelodendridae','tnd<Foraminifera',
                  'Acantharea','tnd<Rhizaria','tnd<Acantharea','Coelodendrum','Aulatractus','tnd<Coelodendridae',
                  'colonial<Rhizaria','Aulographis','tnd<Aulosphaeridae',
                  'Cannosphaeridae','colonial<Aulosphaeridae')

  det_names <- c("detritus","feces","house","light<detritus","fiber<detritus",
                 "darksphere","temp circle","aggregates")

  cope_names <- c("Copepoda","Eucalanidae","tnd<Copepoda")

  chae_names <- c("Chaetognatha","tnd<Chaetognatha",
                  'head<Chaetognatha','tail<Chaetognatha')

  worm_names <- c("Tomopteridae","Poeobius","Alciopidae","Annelida")

  jell_names <- c("Cnidaria<Hydrozoa","Cnidaria<Metazoa","Salpida",
                  'Pelagia','Ctenophora<Metazoa','Siphonophorae')

  trich_names <- c("puff","tuff","like<Trichodesmium","tnd<Trichodesmium")

  spl_names <- c("Eumalacostraca","tnd<Eumalacostraca","Crustacea")


  if(obj_name %in% rhiz_names){
    new_name <- 'Rhizaria' #rename rhizaria
  } else if (obj_name %in% det_names) {
    new_name <- 'detritus' #rename rhizaria
  } else if (obj_name %in% cope_names) {
    new_name <- 'Copepoda' #rename rhizaria
  } else if (obj_name %in% chae_names) {
    new_name <- 'Chaetognatha' #rename rhizaria
  } else if(obj_name %in% worm_names) {
    new_name <- 'Annelida' #rename rhizaria
  } else if(obj_name %in% jell_names) {
    new_name <- 'Gelatinous' #rename rhizaria
  } else if(obj_name %in% trich_names) {
    new_name <- 'Trichodesmium' #rename rhizaria
  } else if(obj_name %in% spl_names) {
    new_name <- 'Shrimp-like' #rename rhizaria
  } else if(obj_name == 'Ostracoda') {
    new_name <- 'Ostra/Clado'
  } else if(obj_name == 'Pteropoda') {
    new_name <- 'Mollusca'
  } else if(obj_name == 'other<living') {
    new_name <- 'Other-living'
  } else {
    new_name <- obj_name
  }

  return(new_name)
}

#' uvp_renamer
uvp_renamer <- function(vect) {
  new_vect <- unlist(lapply(vect,name_selector))
  return(new_vect)
}
