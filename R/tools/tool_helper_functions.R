###
# Helper functions
###

#' na.rm() a function to remove na's from a vector
#'
#' @param x is a numeric vector
na.rm <- function(x){
  ret <- x[which(!is.na(x))];
  return(ret);
}
