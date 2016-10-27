## Script for launching Scleroderma web application

ensure_pkg <- function(pkg) {
  ## Load a package; install it first if necessary.
  ##
  ## Input
  ## -----
  ##
  ## pkg : A string specifying the package.
  ##
  ## Output
  ## ------
  ##
  ## Nothing, used for side-effects.
  ##
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) {
      stop(sprintf("%s is missing and could not be installed", pkg))
    }
  }
}

options(repos = c(CRAN = "http://cran.rstudio.com/"))

packages <- c(
  "shiny"
, "shinydashboard"
, "ggplot2"
, "reshape2"
, "readr"
, "dplyr"
, "RColorBrewer"
, "mvtnorm"
, "RJDBC"
, "DBI"
)

invisible(lapply(packages, ensure_pkg))

shiny::runUrl("https://www.dropbox.com/s/emzqbua2rj7tltp/Scleroderma_WebApp.zip", launch.browser = TRUE, quiet =TRUE)
