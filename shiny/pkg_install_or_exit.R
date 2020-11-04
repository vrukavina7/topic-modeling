# script for installing needed packages

#' Install R package.
#'
#' Install R package or exit if something went wrong.
#' @param pkg  A package to install.
#' @export
install_func <- function(pkg) {
  install.packages(pkg)
  if ( !library(pkg, character.only=TRUE, logical.return=TRUE)) {
    quit(status=1, save='no')
  }
}


# load and install packages from requirements.txt
filepath <- commandArgs(trailingOnly = TRUE)[1]
requirements_file <- file(filepath, open = 'r')
while (length(pkg <- readLines(requirements_file, n = 1, warn = FALSE)) > 0) {
     install_func(pkg = pkg)
}
close(requirements_file)
