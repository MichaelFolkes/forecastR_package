

#' @title Package load or install and load
#'
#' @description For installing and/or loading R packages
#'
#' @param package_names Character string of length one or more. The package name.
#'
#' @return Nothing returned.
#' @export
#'
#' @examples
load_or_install <- function(package_names){
	# function to load/install required packages
	for(package_name in package_names){
		if(!is_installed(package_name)){install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")}
		library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
	}
}#END load_or_install

